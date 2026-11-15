// vkframes' web viewer: draws a scene exported by WindowProxy%export_web
// (vkf_web_writer.f90) with Three.js. The page holds the scene (JSON, in
// <script id="vkf-scene">), its arrays (base64, <script id="vkf-data">) and its
// assets (<script id="vkf-asset-N">); this script is inlined after them.
//
// The scene: a tree of frames (each an Object3D with its pose, its motion over
// time as a track of poses, its decorations and what it draws: lines, points,
// meshes, spheres, models), views (the camera kept in each view's frame, as
// vkframes' trackball), the sky (stars, constellations, a texture) and HUD text.
// Lines are drawn by their own shader: wide (window units), cut by time (trace
// mode, trails with fading), each chunk placed at its own origin so that
// Three.js composes the transforms in double precision.
//
// Mouse: drag to turn, right-drag (or shift-drag) to pan, wheel to zoom;
// touch: drag to turn, pinch to zoom, two fingers to pan;
// keys: v / V the next / previous view, space resets the view, p plays or
// pauses. URL options (for tests): ?t=<time> the time, ?view=<n> the view,
// ?camera=<eye,center,up> its camera, ?paused. window.vkfReady is true once the page has drawn with its models.

import * as THREE from 'three';

THREE.ColorManagement.enabled = false;    // colors as given (as vkframes draws them)

const S = JSON.parse(document.getElementById('vkf-scene').textContent);
const params = new URLSearchParams(location.search);

// ---------------------------------------------------------------- data

async function decode(b64, mime = 'application/octet-stream') {
    b64 = b64.trim();
    if (b64.length === 0) return new ArrayBuffer(0);
    const response = await fetch(`data:${mime};base64,${b64}`);
    return await response.arrayBuffer();
}
const bin = await decode(document.getElementById('vkf-data').textContent);
const f32 = (r) => new Float32Array(bin, r[0], r[1]);
const f64 = (r) => new Float64Array(bin, r[0], r[1]);
const u32 = (r) => new Uint32Array(bin, r[0], r[1]);

function assetElement(id) { return document.getElementById(`vkf-asset-${id}`); }
function assetURL(id) {
    const el = assetElement(id);
    return el ? `data:${el.dataset.mime};base64,${el.textContent.trim()}` : null;
}
const textureCache = new Map();
function texture(id) {
    if (id === undefined || id < 0) return null;
    if (!textureCache.has(id)) {
        const url = assetURL(id);
        if (!url) return null;
        const tex = new THREE.TextureLoader().load(url, () => { pending.delete(tex); });
        pending.add(tex);
        tex.anisotropy = 8;
        textureCache.set(id, tex);
    }
    return textureCache.get(id);
}
const pending = new Set();      // textures and models still loading (window.vkfReady once none)

// ---------------------------------------------------------------- page

const style = document.createElement('style');
style.textContent = `
:root { --panel: rgba(18, 22, 30, 0.78); --text: #e8ecf2; --muted: #9aa4b2; --accent: #6fb3ff; }
html, body { margin: 0; height: 100%; overflow: hidden; background: #000; color: var(--text);
  font: 13px/1.3 system-ui, -apple-system, "Segoe UI", sans-serif; }
canvas { display: block; touch-action: none; }
#vkf-labels, #vkf-hud { position: fixed; inset: 0; pointer-events: none; overflow: hidden; }
.vkf-label { position: absolute; white-space: pre; text-shadow: 0 0 3px #000, 0 0 2px #000; }
.vkf-hud { position: absolute; white-space: pre; }
#vkf-bar { position: fixed; left: 12px; right: 12px; bottom: 12px; display: flex; gap: 10px; align-items: center;
  padding: 8px 12px; background: var(--panel); border-radius: 10px; backdrop-filter: blur(6px);
  -webkit-backdrop-filter: blur(6px); flex-wrap: wrap; }
#vkf-bar button, #vkf-bar select { font: inherit; color: var(--text); background: rgba(255,255,255,0.08);
  border: 1px solid rgba(255,255,255,0.16); border-radius: 6px; padding: 4px 10px; cursor: pointer; }
#vkf-bar button:hover, #vkf-bar select:hover { background: rgba(255,255,255,0.16); }
#vkf-play { min-width: 4.5em; }
#vkf-slider { flex: 1 1 200px; min-width: 120px; accent-color: var(--accent); }
#vkf-time { font-variant-numeric: tabular-nums; min-width: 10em; }
#vkf-help { color: var(--muted); font-size: 12px; margin-left: auto; }
@media (max-width: 640px) { #vkf-help { display: none; } }
`;
document.head.appendChild(style);
document.title = S.title || 'vkframes';

const renderer = new THREE.WebGLRenderer({ antialias: true, logarithmicDepthBuffer: true,
                                           preserveDrawingBuffer: true });
renderer.outputColorSpace = THREE.LinearSRGBColorSpace;
renderer.setPixelRatio(window.devicePixelRatio || 1);
renderer.setSize(window.innerWidth, window.innerHeight);
renderer.autoClear = false;
const bg = S.background || [0, 0, 0];
renderer.setClearColor(new THREE.Color(bg[0], bg[1], bg[2]), 1);
document.body.appendChild(renderer.domElement);
const labelLayer = document.createElement('div');
labelLayer.id = 'vkf-labels';
document.body.appendChild(labelLayer);
const hudLayer = document.createElement('div');
hudLayer.id = 'vkf-hud';
document.body.appendChild(hudLayer);

const scene = new THREE.Scene();
const camera = new THREE.PerspectiveCamera(45, 1, 1e-3, 1e9);
camera.matrixAutoUpdate = false;
camera.matrixWorldAutoUpdate = false;
const orthoCamera = new THREE.OrthographicCamera(-1, 1, 1, -1, 1e-3, 1e9);
orthoCamera.matrixAutoUpdate = false;
orthoCamera.matrixWorldAutoUpdate = false;

const css = (c) => `rgba(${Math.round(255 * c[0])}, ${Math.round(255 * c[1])}, ${Math.round(255 * c[2])}, ${c.length > 3 ? c[3] : 1})`;
const color3 = (c) => new THREE.Color(c[0], c[1], c[2]);

// ---------------------------------------------------------------- shaders

const lineMaterials = [], pointMaterials = [], meshMaterials = [];

const lineVertex = /* glsl */`
#include <common>
#include <logdepthbuf_pars_vertex>
attribute vec2 corner;
attribute vec3 pa, pb;
#ifdef USE_TIME
attribute float ta, tb;
#endif
#ifdef USE_VCOL
attribute vec4 ca, cb;
#endif
uniform vec2 resolution;
uniform float width, now, trace, trailOn;
uniform vec2 trail;
uniform vec4 lineColor;
varying vec4 vColor;
varying float vT;
void cull() { gl_Position = vec4(2.0, 2.0, 2.0, 1.0); vColor = vec4(0.0); vT = 0.0; }
void main() {
    float s0 = 0.0, s1 = 1.0, t0 = 0.0, t1 = 0.0;
#ifdef USE_TIME
    t0 = ta; t1 = tb;
    float lo = -1.0e30, hi = 1.0e30;
    if (trailOn > 0.5) { lo = now - trail.x; hi = now + trail.y; }
    else if (trace > 0.5) { hi = now; }
    float dt = t1 - t0;
    if (abs(dt) <= 0.0) {
        if (t0 < lo || t0 > hi) { cull(); return; }
    } else {
        float sa = (lo - t0) / dt, sb = (hi - t0) / dt;
        s0 = max(0.0, min(sa, sb));
        s1 = min(1.0, max(sa, sb));
        if (s0 >= s1) { cull(); return; }
    }
#endif
    vec4 va = modelViewMatrix * vec4(mix(pa, pb, s0), 1.0);
    vec4 vb = modelViewMatrix * vec4(mix(pa, pb, s1), 1.0);
    if (isPerspectiveMatrix(projectionMatrix)) {           // clipped at the near plane
        float zn = -1.0001 * projectionMatrix[3][2] / (projectionMatrix[2][2] - 1.0);
        if (va.z > zn && vb.z > zn) { cull(); return; }
        if (va.z > zn) { float f = (zn - va.z) / (vb.z - va.z); va = mix(va, vb, f); s0 = mix(s0, s1, f); }
        else if (vb.z > zn) { float f = (zn - vb.z) / (va.z - vb.z); vb = mix(vb, va, f); s1 = mix(s1, s0, f); }
    }
    vec4 qa = projectionMatrix * va, qb = projectionMatrix * vb;
    vec2 na = qa.xy / qa.w, nb = qb.xy / qb.w;
    vec2 dir = (nb - na) * resolution;
    float len = length(dir);
    dir = len > 1.0e-6 ? dir / len : vec2(1.0, 0.0);
    vec2 nrm = vec2(-dir.y, dir.x);
    bool atA = corner.x < 0.5;
    vec4 q = atA ? qa : qb;
    vec2 off = (nrm * corner.y + dir * (atA ? -1.0 : 1.0)) * 0.5 * width;   // (pixels; ends extended: joins)
    q.xy += off * 2.0 / resolution * q.w;
    gl_Position = q;
    float s = atA ? s0 : s1;
    vT = mix(t0, t1, s);
#ifdef USE_VCOL
    vColor = mix(ca, cb, s);
#else
    vColor = lineColor;
#endif
    #include <logdepthbuf_vertex>
}`;

const lineFragment = /* glsl */`
#include <logdepthbuf_pars_fragment>
uniform float now, trailOn, fade;
uniform vec2 trail, replace, opacity;
uniform vec4 pastColor, futureColor;
varying vec4 vColor;
varying float vT;
void main() {
    vec4 c = vColor;
    if (trailOn > 0.5) {
        if (vT <= now) {
            if (replace.x > 0.5) c = pastColor;
            c.a *= opacity.x;
            float f = fade * trail.x;
            if (f > 0.0) c.a *= clamp((trail.x - (now - vT)) / f, 0.0, 1.0);
        } else {
            if (replace.y > 0.5) c = futureColor;
            c.a *= opacity.y;
            float f = fade * trail.y;
            if (f > 0.0) c.a *= clamp((trail.y - (vT - now)) / f, 0.0, 1.0);
        }
    }
    if (c.a <= 0.0) discard;
    gl_FragColor = c;
    #include <logdepthbuf_fragment>
}`;

const pointVertex = /* glsl */`
#include <common>
#include <logdepthbuf_pars_vertex>
#ifdef USE_TIME
attribute float time;
#endif
#ifdef USE_VCOL
attribute vec4 vcol;
#endif
uniform float size, pixelRatio, attenuation, now, trace;
uniform vec4 pointColor;
varying vec4 vColor;
void main() {
    vec4 mv = modelViewMatrix * vec4(position, 1.0);
    gl_Position = projectionMatrix * mv;
    float s = size * pixelRatio;
    if (attenuation > 0.0 && isPerspectiveMatrix(projectionMatrix)) s *= min(1.0, attenuation / max(-mv.z, 1.0e-30));
#ifdef USE_TIME
    if (trace > 0.5 && time > now) { gl_Position = vec4(2.0, 2.0, 2.0, 1.0); s = 0.0; }
#endif
    gl_PointSize = s;
#ifdef USE_VCOL
    vColor = vcol;
#else
    vColor = pointColor;
#endif
    #include <logdepthbuf_vertex>
}`;

const pointFragment = /* glsl */`
#include <logdepthbuf_pars_fragment>
uniform int shape;
uniform float clock;
varying vec4 vColor;
void main() {
    vec2 p = gl_PointCoord * 2.0 - 1.0;
    float r = length(p);
    float a = 1.0;
    float e = 0.12;                                       // (edge smoothing)
    if (shape == 2) { a = 1.0; }                                                    // square
    else if (shape == 1) { a = smoothstep(0.62 - e, 0.62, r) * (1.0 - smoothstep(1.0 - e, 1.0, r)); } // circle
    else if (shape == 3) { a = (abs(p.x) < 0.18 || abs(p.y) < 0.18) ? 1.0 : 0.0; }  // plus
    else if (shape == 4) { a = (abs(p.x - p.y) < 0.26 || abs(p.x + p.y) < 0.26) && r <= 1.0 ? 1.0 : 0.0; } // x
    else if (shape == 5) { a = max(smoothstep(0.7 - e, 0.7, r) * (1.0 - smoothstep(1.0 - e, 1.0, r)),
                               1.0 - smoothstep(0.3 - e, 0.3, r)); }               // target
    else if (shape == 7) { float rr = 0.35 + 0.6 * fract(clock); a = 1.0 - smoothstep(0.0, 0.15, abs(r - rr)); } // circle pulse
    else if (shape == 8) { a = (1.0 - smoothstep(1.0 - e, 1.0, r)) * (0.55 + 0.45 * sin(6.2831853 * clock)); } // disk pulse
    else { a = 1.0 - smoothstep(1.0 - e, 1.0, r); }                                // disk (and others)
    vec4 c = vColor;
    c.a *= a;
    if (c.a <= 0.01) discard;
    gl_FragColor = c;
    #include <logdepthbuf_fragment>
}`;

const meshVertex = /* glsl */`
#include <common>
#include <logdepthbuf_pars_vertex>
#ifdef USE_TIME
attribute float time;
varying float vT;
#endif
#ifdef USE_VCOL
attribute vec4 vcol;
#endif
uniform vec4 meshColor;
varying vec4 vColor;
void main() {
    gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
#ifdef USE_TIME
    vT = time;
#endif
#ifdef USE_VCOL
    vColor = vcol * meshColor;
#else
    vColor = meshColor;
#endif
    #include <logdepthbuf_vertex>
}`;

const meshFragment = /* glsl */`
#include <logdepthbuf_pars_fragment>
uniform float now, trace;
varying vec4 vColor;
#ifdef USE_TIME
varying float vT;
#endif
void main() {
#ifdef USE_TIME
    if (trace > 0.5 && vT > now) discard;
#endif
    gl_FragColor = vColor;
    #include <logdepthbuf_fragment>
}`;

const vec4of = (c) => new THREE.Vector4(c[0], c[1], c[2], c.length > 3 ? c[3] : 1);

function lineMaterial(item, timed, colored) {
    const tr = item.trail;
    const translucent = (item.color[3] ?? 1) < 1 || !!tr || colored;
    const m = new THREE.ShaderMaterial({
        vertexShader: lineVertex, fragmentShader: lineFragment,
        defines: Object.assign({}, timed ? { USE_TIME: '' } : {}, colored ? { USE_VCOL: '' } : {}),
        uniforms: {
            resolution: { value: new THREE.Vector2(1, 1) }, width: { value: item.width || 1 },
            now: { value: 0 }, trace: { value: item.trace ? 1 : 0 }, trailOn: { value: tr ? 1 : 0 },
            trail: { value: new THREE.Vector2(tr ? tr.past : 0, tr ? tr.future : 0) }, fade: { value: tr ? tr.fade : 0 },
            replace: { value: new THREE.Vector2(tr ? tr.replace[0] : 0, tr ? tr.replace[1] : 0) },
            opacity: { value: new THREE.Vector2(tr ? tr.opacity[0] : 1, tr ? tr.opacity[1] : 1) },
            pastColor: { value: vec4of(tr ? tr.colors.slice(0, 4) : [1, 1, 1, 1]) },
            futureColor: { value: vec4of(tr ? tr.colors.slice(4, 8) : [1, 1, 1, 1]) },
            lineColor: { value: vec4of(item.color) },
        },
        transparent: translucent, depthWrite: !translucent, side: THREE.DoubleSide,
    });
    m.userData.width = item.width || 1;
    lineMaterials.push(m);
    return m;
}

const quadIndex = [0, 1, 2, 2, 1, 3];
const quadCorners = new Float32Array([0, -1, 0, 1, 1, -1, 1, 1]);

// A line item: a mesh of instanced quads per chunk (a strip: point i to i+1;
// segments: points 2i-1 to 2i), the chunk at its origin.
function buildLines(item) {
    const group = new THREE.Group();
    const timed = item.chunks.some((c) => c.t), colored = item.chunks.some((c) => c.c);
    const material = lineMaterial(item, timed, colored);
    for (const c of item.chunks) {
        const p = f32(c.p), n = p.length / 3;
        const count = item.segments ? Math.floor(n / 2) : n - 1;
        if (count < 1) continue;
        const g = new THREE.InstancedBufferGeometry();
        g.setIndex(quadIndex);
        g.setAttribute('corner', new THREE.BufferAttribute(quadCorners, 2));
        const step = item.segments ? 2 : 1;
        const pb = new THREE.InstancedInterleavedBuffer(p, 3 * step);
        g.setAttribute('pa', new THREE.InterleavedBufferAttribute(pb, 3, 0));
        g.setAttribute('pb', new THREE.InterleavedBufferAttribute(pb, 3, 3));
        if (timed) {
            const t = c.t ? f32(c.t) : new Float32Array(n);
            const tb = new THREE.InstancedInterleavedBuffer(t, step);
            g.setAttribute('ta', new THREE.InterleavedBufferAttribute(tb, 1, 0));
            g.setAttribute('tb', new THREE.InterleavedBufferAttribute(tb, 1, 1));
        }
        if (colored) {
            const col = c.c ? f32(c.c) : new Float32Array(4 * n).fill(1);
            const cb = new THREE.InstancedInterleavedBuffer(col, 4 * step);
            g.setAttribute('ca', new THREE.InterleavedBufferAttribute(cb, 4, 0));
            g.setAttribute('cb', new THREE.InterleavedBufferAttribute(cb, 4, 4));
        }
        g.instanceCount = count;
        const mesh = new THREE.Mesh(g, material);
        mesh.frustumCulled = false;
        mesh.position.fromArray(c.o);
        group.add(mesh);
    }
    return group;
}

function buildPoints(item) {
    const group = new THREE.Group();
    const timed = item.chunks.some((c) => c.t), colored = item.chunks.some((c) => c.c);
    const translucent = (item.color[3] ?? 1) < 1 || colored;
    const material = new THREE.ShaderMaterial({
        vertexShader: pointVertex, fragmentShader: pointFragment,
        defines: Object.assign({}, timed ? { USE_TIME: '' } : {}, colored ? { USE_VCOL: '' } : {}),
        uniforms: {
            size: { value: item.size || 10 }, pixelRatio: { value: 1 }, attenuation: { value: item.attenuation || 0 },
            now: { value: 0 }, trace: { value: item.trace ? 1 : 0 }, pointColor: { value: vec4of(item.color) },
            shape: { value: item.shape || 0 }, clock: { value: 0 },
        },
        transparent: true, depthWrite: !translucent,
    });
    pointMaterials.push(material);
    for (const c of item.chunks) {
        const g = new THREE.BufferGeometry();
        g.setAttribute('position', new THREE.BufferAttribute(f32(c.p), 3));
        if (c.t) g.setAttribute('time', new THREE.BufferAttribute(f32(c.t), 1));
        if (c.c) g.setAttribute('vcol', new THREE.BufferAttribute(f32(c.c), 4));
        const pts = new THREE.Points(g, material);
        pts.frustumCulled = false;
        pts.position.fromArray(c.o);
        group.add(pts);
    }
    return group;
}

function buildMesh(item) {
    const g = new THREE.BufferGeometry();
    g.setAttribute('position', new THREE.BufferAttribute(f32(item.p), 3));
    g.setIndex(new THREE.BufferAttribute(u32(item.i), 1));
    let material;
    if (item.lit) {
        g.setAttribute('normal', new THREE.BufferAttribute(f32(item.n), 3));
        if (item.c) g.setAttribute('color', new THREE.BufferAttribute(f32(item.c), 4));
        const a = item.color[3] ?? 1;
        material = new THREE.MeshPhongMaterial({
            color: color3(item.color), opacity: a, transparent: a < 1 || !!item.c, vertexColors: !!item.c,
            side: item.two_sided ? THREE.DoubleSide : THREE.FrontSide, shininess: 0, specular: 0x000000,
        });
    } else {
        if (item.c) g.setAttribute('vcol', new THREE.BufferAttribute(f32(item.c), 4));
        if (item.t) g.setAttribute('time', new THREE.BufferAttribute(f32(item.t), 1));
        material = new THREE.ShaderMaterial({
            vertexShader: meshVertex, fragmentShader: meshFragment,
            defines: Object.assign({}, item.t ? { USE_TIME: '' } : {}, item.c ? { USE_VCOL: '' } : {}),
            uniforms: { meshColor: { value: vec4of(item.color) }, now: { value: 0 }, trace: { value: item.trace ? 1 : 0 } },
            transparent: true, depthWrite: false, side: THREE.DoubleSide,
        });
        meshMaterials.push(material);
    }
    const mesh = new THREE.Mesh(g, material);
    mesh.frustumCulled = false;
    mesh.position.fromArray(item.o);
    return mesh;
}

const sphereGeometry = new THREE.SphereGeometry(1, 192, 96).rotateX(Math.PI / 2);   // poles on z, u = 0.5 at +x

function buildSphere(item) {
    const map = texture(item.texture);
    const d = item.diffuse || [1, 1, 1, 1];
    const c = map ? d : [d[0] * item.color[0], d[1] * item.color[1], d[2] * item.color[2]];
    const opacity = item.opacity ?? 1;
    const sp = item.specular || [0, 0, 0], em = item.emission || [0, 0, 0];
    const material = new THREE.MeshPhongMaterial({
        map, color: color3(c), specular: color3(sp), shininess: Math.max(item.shininess || 0, 1),
        emissive: color3(em), transparent: opacity < 1, opacity,
    });
    if ((item.shininess || 0) <= 0) material.specular = new THREE.Color(0, 0, 0);
    const mesh = new THREE.Mesh(sphereGeometry, material);
    mesh.userData.body = true;                  // (hides the labels behind it: placeLabels)
    mesh.scale.fromArray(item.radii);
    mesh.position.fromArray(item.center);
    mesh.quaternion.fromArray(item.q);
    return mesh;
}

const modelCache = new Map();
let gltfLoader = null;
function loadModel(id) {
    if (!modelCache.has(id)) {
        const promise = (async () => {
            if (!gltfLoader) {
                const { GLTFLoader } = await import('three/addons/loaders/GLTFLoader.js');
                gltfLoader = new GLTFLoader();
            }
            const el = assetElement(id);
            const data = await decode(el.textContent, el.dataset.mime);
            return await new Promise((resolve, reject) => gltfLoader.parse(data, '', resolve, reject));
        })();
        modelCache.set(id, promise);
    }
    return modelCache.get(id);
}

function buildModel(item) {
    const outer = new THREE.Group();
    outer.matrixAutoUpdate = false;
    outer.matrix.fromArray(item.matrix);
    if (item.model >= 0) {
        const upright = new THREE.Group();      // (glTF is y up; vkframes reads it z up, as vsgXchange does)
        upright.rotation.x = Math.PI / 2;
        outer.add(upright);
        const token = {};
        pending.add(token);
        loadModel(item.model).then((gltf) => { upright.add(gltf.scene.clone(true)); pending.delete(token); })
            .catch((e) => { console.error('vkframes: a model could not be read', e); pending.delete(token); });
    } else {
        const [cx, cy, cz, r] = item.bound;
        const box = new THREE.Mesh(new THREE.BoxGeometry(2 * r, 2 * r, 2 * r),
                                   new THREE.MeshBasicMaterial({ color: color3(item.color), wireframe: true }));
        box.position.set(cx, cy, cz);
        outer.add(box);
    }
    return outer;
}

function buildItem(item) {
    switch (item.type) {
        case 'lines': return buildLines(item);
        case 'points': return buildPoints(item);
        case 'mesh': return buildMesh(item);
        case 'sphere': return buildSphere(item);
        case 'model': return buildModel(item);
    }
    return null;
}

// ---------------------------------------------------------------- frames

const labels = [];          // { el, object, offset (local), onTop }
function addLabel(text, object, offset, rgba, size, alignment, onTop = false) {
    if (!text) return;
    const el = document.createElement('div');
    el.className = 'vkf-label';
    el.textContent = text;
    el.style.color = css(rgba);
    el.style.fontSize = `${size}px`;
    el.style.transform = alignTransform(alignment);
    labelLayer.appendChild(el);
    labels.push({ el, object, offset: new THREE.Vector3().fromArray(offset), onTop });
}

// vkframes' ALIGN_* (the point of the text at the anchor) as a CSS translation
function alignTransform(a) {
    const h = [0, 0, 0, -50, -50, -50, -100, -100, -100, 0, -50, -100][a] ?? 0;
    const v = [0, -50, -100, 0, -50, -100, 0, -50, -100, -80, -80, -80][a] ?? -80;
    return `translate(${h}%, ${v}%)`;
}

function buildAxes(f, node) {
    const g = f.axes_geometry;
    const rgba = f.color;
    const material = new THREE.MeshPhongMaterial({ color: color3(rgba), opacity: rgba[3] ?? 1, transparent: (rgba[3] ?? 1) < 1 });
    for (let k = 0; k < 3; k++) {
        const bit = 1 << k;
        const [bx, by, bz, len, head, rb, rh] = g.slice(7 * k, 7 * k + 7);
        const dir = new THREE.Vector3(k === 0 ? 1 : 0, k === 1 ? 1 : 0, k === 2 ? 1 : 0);
        const base = new THREE.Vector3(bx, by, bz);
        if (f.axes & bit) {
            const turn = new THREE.Quaternion().setFromUnitVectors(new THREE.Vector3(0, 1, 0), dir);
            const body = new THREE.Mesh(new THREE.CylinderGeometry(rb, rb, (1 - head) * len, 24), material);
            body.quaternion.copy(turn);
            body.position.copy(base).addScaledVector(dir, 0.5 * (1 - head) * len);
            const tip = new THREE.Mesh(new THREE.ConeGeometry(rh, head * len, 24), material);
            tip.quaternion.copy(turn);
            tip.position.copy(base).addScaledVector(dir, (1 - 0.5 * head) * len);
            node.add(body, tip);
        }
        if (f.axes_labels & bit) {
            const at = base.clone().addScaledVector(dir, len);
            addLabel(f.axis_names[k], node, at.toArray(), rgba, f.label_size, f.label_alignment);
        }
    }
    if (f.name_label) addLabel(f.name, node, [0, 0, 0], rgba, f.label_size, f.label_alignment, !!f.name_on_top);
}

const frames = S.frames.map((f) => ({ data: f, instances: [], items: null, track: null }));
for (const fr of frames) {
    const t = fr.data.track;
    if (t) fr.track = { t: f64(t.t), p: f64(t.p), q: f32(t.q) };
}

// A frame's object (one per place it has in the tree: a frame with several
// parents is drawn under each), with its decorations, items and children.
function instance(id, depth = 0) {
    const fr = frames[id], f = fr.data;
    const node = new THREE.Group();
    node.name = f.name;
    node.position.fromArray(f.p);
    node.quaternion.fromArray(f.q);
    const contents = new THREE.Group();
    contents.visible = f.contents;
    node.add(contents);
    if (!fr.items) fr.items = f.items.map(buildItem).filter((x) => x);
    for (const obj of fr.items) contents.add(fr.instances.length === 0 ? obj : obj.clone(true));
    buildAxes(f, node);
    if (f.light) {
        const light = new THREE.PointLight(color3(f.light.diffuse), Math.PI, 0, 0);
        node.add(light);
        scene.add(new THREE.AmbientLight(color3(f.light.ambient), Math.PI));
    }
    fr.instances.push(node);
    if (depth < 64) for (const c of f.children) node.add(instance(c, depth + 1));
    return node;
}
scene.add(instance(0));

// the window's headlight (from the eye), unless a frame carries a light
const light = S.light || { headlight: true, ambient: [0.2, 0.2, 0.2], diffuse: [1, 1, 1] };
let headlight = null;
if (light.headlight) {
    scene.add(new THREE.AmbientLight(color3(light.ambient), Math.PI));
    headlight = new THREE.DirectionalLight(color3(light.diffuse), Math.PI);
    scene.add(headlight, headlight.target);
}

// ---------------------------------------------------------------- time

const T = S.time || { ref: 0, now: 0, start: 0, end: 0, animate: false, scale: 1 };
let now = T.now;                // the scene's time (absolute, as in the program)
if (params.has('t')) now = Number(params.get('t'));
let playing = T.animate && !T.paused && !params.has('paused');
let speed = 1;

function pose(track, tau, pos, quat) {
    const t = track.t, n = t.length;
    let i = 0;
    if (tau <= t[0]) i = 0;
    else if (tau >= t[n - 1]) i = n - 2;
    else {
        let lo = 0, hi = n - 1;
        while (hi - lo > 1) { const mid = (lo + hi) >> 1; if (t[mid] <= tau) lo = mid; else hi = mid; }
        i = lo;
    }
    if (n === 1) {
        pos.set(track.p[0], track.p[1], track.p[2]);
        quat.set(track.q[0], track.q[1], track.q[2], track.q[3]);
        return;
    }
    const s = Math.min(1, Math.max(0, (tau - t[i]) / (t[i + 1] - t[i] || 1)));
    const p = track.p, q = track.q;
    pos.set(p[3 * i] + s * (p[3 * i + 3] - p[3 * i]), p[3 * i + 1] + s * (p[3 * i + 4] - p[3 * i + 1]),
            p[3 * i + 2] + s * (p[3 * i + 5] - p[3 * i + 2]));
    qa.set(q[4 * i], q[4 * i + 1], q[4 * i + 2], q[4 * i + 3]);
    qb.set(q[4 * i + 4], q[4 * i + 5], q[4 * i + 6], q[4 * i + 7]);
    quat.slerpQuaternions(qa, qb, s);
}
const qa = new THREE.Quaternion(), qb = new THREE.Quaternion();

function applyTime() {
    const tau = now - T.ref;
    for (const fr of frames) {
        if (!fr.track) continue;
        for (const node of fr.instances) pose(fr.track, tau, node.position, node.quaternion);
    }
    for (const m of lineMaterials) m.uniforms.now.value = tau;
    for (const m of pointMaterials) if (m.uniforms.now) m.uniforms.now.value = tau;
    for (const m of meshMaterials) m.uniforms.now.value = tau;
}

// ---------------------------------------------------------------- views

const views = (S.views && S.views.length ? S.views : [{ frame: 0, lookat: -1, frame_type: 1, rotation: 1,
    projection: 1, fovy: 45, home: [0, -10, 0, 0, 0, 0, 0, 0, 1], name: 'View' }]).map((v) => {
    const c = v.camera || v.home;
    return { data: v, eye: new THREE.Vector3(c[0], c[1], c[2]), center: new THREE.Vector3(c[3], c[4], c[5]),
             up: new THREE.Vector3(c[6], c[7], c[8]).normalize() };
});
let current = Math.min(views.length - 1, Math.max(0, Number(params.get('view') ?? S.view ?? 0)));
if (params.has('camera')) {     // ?camera=eye,center,up (9 numbers, in the view's frame)
    const c = params.get('camera').split(',').map(Number);
    if (c.length === 9 && c.every(Number.isFinite)) {
        views[current].eye.set(c[0], c[1], c[2]);
        views[current].center.set(c[3], c[4], c[5]);
        views[current].up.set(c[6], c[7], c[8]).normalize();
    }
}

function resetView(v) {
    const h = v.data.home;
    v.eye.set(h[0], h[1], h[2]);
    v.center.set(h[3], h[4], h[5]);
    v.up.set(h[6], h[7], h[8]).normalize();
}

const Y = new THREE.Vector3(0, 1, 0);
// The transform from the root frame to the view's frame (vkf_view's world_to_view).
function worldToView(v) {
    const m = new THREE.Matrix4();
    const fr = frames[v.data.frame];
    if (!fr || fr.instances.length === 0) return m;
    const w = fr.instances[0].matrixWorld;
    if (v.data.frame_type === 0) {          // ABSOLUTE_FRAME: the root's axes at the frame's origin
        const o = new THREE.Vector3().setFromMatrixPosition(w);
        m.makeTranslation(-o.x, -o.y, -o.z);
    } else {
        m.copy(w).invert();
    }
    const la = frames[v.data.lookat];
    if (v.data.lookat < 0 || !la || la.instances.length === 0) return m;
    const rel = new THREE.Vector3().setFromMatrixPosition(la.instances[0].matrixWorld).applyMatrix4(m);
    if (rel.length() <= 1e-6) return m;
    const turn = (from) => new THREE.Matrix4().makeRotationFromQuaternion(
        new THREE.Quaternion().setFromUnitVectors(from.clone().normalize(), Y));
    if (v.data.rotation === 0) {
        m.premultiply(turn(rel));
    } else {
        const xy = new THREE.Vector3(rel.x, rel.y, 0), len = xy.length();
        if (len > 1e-6) m.premultiply(turn(xy));
        m.premultiply(turn(new THREE.Vector3(0, len, rel.z)));
    }
    return m;
}

const look = new THREE.Matrix4();
function placeCamera() {
    const v = views[current];
    const w2v = worldToView(v);
    const ortho = v.data.projection === 0;
    const cam = ortho ? orthoCamera : camera;
    look.lookAt(v.eye, v.center, v.up);
    look.setPosition(v.eye);
    cam.matrixWorld.copy(w2v).invert().multiply(look);
    cam.matrixWorldInverse.copy(cam.matrixWorld).invert();
    const d = Math.max(v.eye.distanceTo(v.center), 1e-12);
    const aspect = window.innerWidth / Math.max(1, window.innerHeight);
    if (ortho) {
        const [l, r, b, t] = v.data.ortho;
        const zoom = d / Math.max(1e-12, new THREE.Vector3().fromArray(v.data.home).distanceTo(
            new THREE.Vector3().fromArray(v.data.home, 3)));
        const hh = 0.5 * (t - b) * zoom, cx = 0.5 * (l + r) * zoom, cy = 0.5 * (t + b) * zoom;
        cam.left = cx - hh * aspect; cam.right = cx + hh * aspect; cam.top = cy + hh; cam.bottom = cy - hh;
        cam.near = -1e6 * d; cam.far = 1e6 * d;
    } else {
        cam.fov = v.data.fovy || 45;
        cam.aspect = aspect;
        cam.near = 1e-6 * d;
        cam.far = 1e9 * d;
    }
    cam.updateProjectionMatrix();
    if (headlight) {        // (light from the eye, along the view)
        const eyeW = new THREE.Vector3().setFromMatrixPosition(cam.matrixWorld);
        const back = new THREE.Vector3(0, 0, 1).transformDirection(cam.matrixWorld);
        headlight.position.copy(eyeW).add(back);
        headlight.target.position.copy(eyeW);
        headlight.updateMatrixWorld();
        headlight.target.updateMatrixWorld();
    }
    return cam;
}

// ---------------------------------------------------------------- the trackball

// Turn the current view's camera about its center by a drag of (dx, dy) pixels.
function turnBy(dx, dy) {
    const v = views[current];
    const offset = v.eye.clone().sub(v.center);
    const forward = offset.clone().negate().normalize();
    const right = forward.clone().cross(v.up).normalize();
    const up = right.clone().cross(forward).normalize();
    const k = 2 * Math.PI / Math.max(300, window.innerHeight);
    const q = new THREE.Quaternion().setFromAxisAngle(up, -dx * k)
        .multiply(new THREE.Quaternion().setFromAxisAngle(right, -dy * k));
    offset.applyQuaternion(q);
    v.up.copy(up).applyQuaternion(q).normalize();
    v.eye.copy(v.center).add(offset);
}

// Move the current view's camera and center sideways with a drag of (dx, dy) pixels.
function panBy(dx, dy) {
    const v = views[current];
    const offset = v.eye.clone().sub(v.center);
    const forward = offset.clone().negate().normalize();
    const right = forward.clone().cross(v.up).normalize();
    const up = right.clone().cross(forward).normalize();
    const k = 2 * Math.tan(0.5 * (v.data.fovy || 45) * Math.PI / 180) * offset.length() / window.innerHeight;
    const move = right.multiplyScalar(-dx * k).addScaledVector(up, dy * k);
    v.eye.add(move);
    v.center.add(move);
}

// Scale the current view's distance from its center (below 1: closer).
function zoomBy(factor) {
    const v = views[current];
    const offset = v.eye.clone().sub(v.center).multiplyScalar(factor);
    v.eye.copy(v.center).add(offset);
}

// Pointers down on the canvas (mouse, pen, each finger): one turns (or pans,
// with the right button or shift); two fingers pinch to zoom and move to pan.
const canvas = renderer.domElement;
const pointers = new Map();          // pointerId -> {x, y}
let drag = null;                     // one pointer: {x, y, pan}
let pinch = null;                    // two: {d, x, y} (the distance between, the midpoint)
function twoPointers() {
    const [a, b] = [...pointers.values()];
    return { d: Math.hypot(a.x - b.x, a.y - b.y), x: 0.5 * (a.x + b.x), y: 0.5 * (a.y + b.y) };
}
function regrip(pan) {               // (the gesture restarted from the pointers down now: no jump)
    drag = null;
    pinch = null;
    if (pointers.size === 1) {
        const p = pointers.values().next().value;
        drag = { x: p.x, y: p.y, pan };
    } else if (pointers.size >= 2) {
        pinch = twoPointers();
    }
}
canvas.addEventListener('contextmenu', (e) => e.preventDefault());
canvas.addEventListener('pointerdown', (e) => {
    try { canvas.setPointerCapture(e.pointerId); } catch { /* (not a live pointer) */ }
    pointers.set(e.pointerId, { x: e.clientX, y: e.clientY });
    regrip(e.button === 2 || e.button === 1 || e.shiftKey);
});
function release(e) {
    if (!pointers.delete(e.pointerId)) return;
    regrip(false);
}
canvas.addEventListener('pointerup', release);
canvas.addEventListener('pointercancel', release);
canvas.addEventListener('pointermove', (e) => {
    const p = pointers.get(e.pointerId);
    if (!p) return;
    p.x = e.clientX;
    p.y = e.clientY;
    if (pinch && pointers.size >= 2) {
        const now2 = twoPointers();
        if (now2.d > 0 && pinch.d > 0) zoomBy(pinch.d / now2.d);
        panBy(now2.x - pinch.x, now2.y - pinch.y);
        pinch = now2;
    } else if (drag) {
        const dx = p.x - drag.x, dy = p.y - drag.y;
        drag.x = p.x;
        drag.y = p.y;
        if (drag.pan) panBy(dx, dy);
        else turnBy(dx, dy);
    }
});
canvas.addEventListener('wheel', (e) => {
    e.preventDefault();
    if (e.ctrlKey) zoomBy(Math.exp(0.01 * e.deltaY));     // (a trackpad's pinch, as the browser sends it)
    else zoomBy(Math.exp(0.0015 * e.deltaY));
}, { passive: false });
// Safari's pinch gestures (e.scale: the spread since the start), never zooming
// the page instead: on a Mac's trackpad they zoom the scene (Chrome and
// Firefox send ctrl+wheel instead); on iOS the touches already do (pointers).
let gestureScale = 1;
document.addEventListener('gesturestart', (e) => { e.preventDefault(); gestureScale = 1; });
document.addEventListener('gesturechange', (e) => {
    e.preventDefault();
    if (pointers.size === 0 && e.scale > 0) zoomBy(gestureScale / e.scale);
    gestureScale = e.scale;
});
document.addEventListener('gestureend', (e) => e.preventDefault());
window.addEventListener('keydown', (e) => {
    if (e.target.tagName === 'SELECT' || e.target.tagName === 'INPUT') return;
    if (e.key === 'v') selectView((current + 1) % views.length);
    else if (e.key === 'V') selectView((current + views.length - 1) % views.length);
    else if (e.key === ' ') { resetView(views[current]); e.preventDefault(); }
    else if (e.key === 'p' && T.animate) setPlaying(!playing);
});

// ---------------------------------------------------------------- controls

const bar = document.createElement('div');
bar.id = 'vkf-bar';
document.body.appendChild(bar);
let playButton = null, slider = null, readout = null, viewSelect = null;
if (T.animate) {
    playButton = document.createElement('button');
    playButton.id = 'vkf-play';
    playButton.addEventListener('click', () => setPlaying(!playing));
    slider = document.createElement('input');
    slider.type = 'range';
    slider.id = 'vkf-slider';
    slider.min = 0;
    slider.max = 10000;
    slider.addEventListener('input', () => {
        now = T.start + (T.end - T.start) * Number(slider.value) / 10000;
        applyTime();
    });
    readout = document.createElement('span');
    readout.id = 'vkf-time';
    const speedSelect = document.createElement('select');
    for (const s of [0.1, 0.25, 0.5, 1, 2, 5, 10, 50, 100]) {
        const o = document.createElement('option');
        o.value = s;
        o.textContent = `×${s}`;
        if (s === 1) o.selected = true;
        speedSelect.appendChild(o);
    }
    speedSelect.title = 'Speed (times the program\'s time scale)';
    speedSelect.addEventListener('change', () => { speed = Number(speedSelect.value); });
    bar.append(playButton, slider, readout, speedSelect);
}
if (views.length > 1) {
    viewSelect = document.createElement('select');
    views.forEach((v, k) => {
        const o = document.createElement('option');
        o.value = k;
        o.textContent = v.data.name || `View ${k + 1}`;
        viewSelect.appendChild(o);
    });
    viewSelect.value = current;
    viewSelect.addEventListener('change', () => selectView(Number(viewSelect.value)));
    bar.appendChild(viewSelect);
}
const reset = document.createElement('button');
reset.textContent = 'Reset view';
reset.addEventListener('click', () => resetView(views[current]));
// (HUD text above the bar)
function placeHud() { hudLayer.style.bottom = `${bar.offsetHeight + 16}px`; }
window.addEventListener('resize', placeHud);
const help = document.createElement('span');
help.id = 'vkf-help';
help.textContent = window.matchMedia('(pointer: coarse)').matches
    ? 'drag: turn · pinch: zoom · two fingers: pan'
    : 'drag: turn · right-drag: pan · wheel: zoom · v: next view';
bar.append(reset, help);
placeHud();

function setPlaying(p) {
    playing = p;
    if (playButton) playButton.textContent = playing ? 'Pause' : 'Play';
}
setPlaying(playing);
function selectView(k) {
    current = k;
    if (viewSelect) viewSelect.value = k;
}

function formatTime(t) {
    const span = Math.abs(T.end - T.start);
    if (span > 2 * 86400) return `t = ${(t / 86400).toFixed(3)} d`;
    if (span > 2 * 3600) return `t = ${(t / 3600).toFixed(3)} h`;
    return `t = ${t.toFixed(span < 20 ? 2 : 1)} s`;
}

// ---------------------------------------------------------------- HUD

for (const h of S.hud || []) {
    const el = document.createElement('div');
    el.className = 'vkf-hud';
    el.textContent = h.text;
    el.style.color = css(h.color);
    el.style.fontSize = `${h.size}px`;
    el.style.left = `${100 * h.position[0]}%`;
    el.style.top = `${100 * (1 - h.position[1])}%`;
    el.style.transform = alignTransform(h.alignment);
    hudLayer.appendChild(el);
}

// ---------------------------------------------------------------- the sky

const skyScene = new THREE.Scene();
const skyCamera = new THREE.PerspectiveCamera(45, 1, 0.01, 100);
skyCamera.matrixAutoUpdate = false;
skyCamera.matrixWorldAutoUpdate = false;
if (S.sky) {
    const sky = new THREE.Group();
    sky.quaternion.fromArray(S.sky.q);
    skyScene.add(sky);
    const map = texture(S.sky.texture);
    if (map) {
        map.wrapS = THREE.RepeatWrapping;
        map.repeat.x = -1;                                 // (seen from inside)
        sky.add(new THREE.Mesh(new THREE.SphereGeometry(50, 96, 48).rotateX(Math.PI / 2),
                               new THREE.MeshBasicMaterial({ map, side: THREE.BackSide, depthWrite: false })));
    }
    if (S.sky.stars) {
        const g = new THREE.BufferGeometry();
        g.setAttribute('position', new THREE.BufferAttribute(f32(S.sky.stars.d), 3));
        g.setAttribute('size', new THREE.BufferAttribute(f32(S.sky.stars.s), 1));
        g.setAttribute('starColor', new THREE.BufferAttribute(f32(S.sky.stars.c), 3));
        const m = new THREE.ShaderMaterial({
            vertexShader: `attribute float size; attribute vec3 starColor; uniform float pixelRatio; varying vec3 vColor;
                void main() { gl_Position = projectionMatrix * modelViewMatrix * vec4(10.0 * position, 1.0);
                gl_PointSize = max(1.0, size * pixelRatio); vColor = starColor; }`,
            fragmentShader: `varying vec3 vColor; void main() { float r = length(gl_PointCoord * 2.0 - 1.0);
                float a = 1.0 - smoothstep(0.5, 1.0, r); if (a <= 0.0) discard; gl_FragColor = vec4(vColor, a); }`,
            uniforms: { pixelRatio: { value: 1 } }, transparent: true, depthWrite: false,
        });
        pointMaterials.push(m);
        const stars = new THREE.Points(g, m);
        stars.frustumCulled = false;
        sky.add(stars);
    }
    if (S.sky.lines) {
        const a = f32(S.sky.lines.a), b = f32(S.sky.lines.b), n = a.length / 3;
        const p = new Float32Array(6 * n);
        for (let i = 0; i < n; i++) {
            for (let k = 0; k < 3; k++) { p[6 * i + k] = 10 * a[3 * i + k]; p[6 * i + 3 + k] = 10 * b[3 * i + k]; }
        }
        const lines = buildLinesFromArray(p, S.sky.lines.color, S.sky.lines.width);
        sky.add(lines);
    }
}
function buildLinesFromArray(p, rgba, width) {
    const item = { color: rgba, width, segments: true, chunks: [] };
    const material = lineMaterial(item, false, false);
    material.depthTest = false;
    const g = new THREE.InstancedBufferGeometry();
    g.setIndex(quadIndex);
    g.setAttribute('corner', new THREE.BufferAttribute(quadCorners, 2));
    const pb = new THREE.InstancedInterleavedBuffer(p, 6);
    g.setAttribute('pa', new THREE.InterleavedBufferAttribute(pb, 3, 0));
    g.setAttribute('pb', new THREE.InterleavedBufferAttribute(pb, 3, 3));
    g.instanceCount = p.length / 6;
    const mesh = new THREE.Mesh(g, material);
    mesh.frustumCulled = false;
    return mesh;
}

// ---------------------------------------------------------------- the frame loop

const size = new THREE.Vector2();
const projected = new THREE.Vector3();
const anchor = new THREE.Vector3(), eye = new THREE.Vector3(), a = new THREE.Vector3(), d = new THREE.Vector3();

// The bodies (spheres' ellipsoids), which hide labels behind them as the
// depth test hides vkframes' labels: each shown, with its transform from the
// root frame to its unit sphere's coordinates.
let bodies = null;
function shownInScene(o) {
    for (; o; o = o.parent) if (!o.visible) return false;
    return true;
}
function visibleBodies() {
    if (!bodies) {
        bodies = [];
        scene.traverse((o) => { if (o.userData.body) bodies.push({ mesh: o, inverse: new THREE.Matrix4() }); });
    }
    const shown = bodies.filter((b) => shownInScene(b.mesh));
    for (const b of shown) b.inverse.copy(b.mesh.matrixWorld).invert();
    return shown;
}

// Whether the segment from the eye to a point (root coordinates) enters a
// body, or the point is inside one.
function hidden(point, shown) {
    for (const b of shown) {
        a.copy(eye).applyMatrix4(b.inverse);
        d.copy(point).applyMatrix4(b.inverse).sub(a);       // (the unit sphere's coordinates)
        const qa = d.lengthSq(), qb = 2 * a.dot(d), qc = a.lengthSq() - 1;
        if (qa + qb + qc < 0) return true;                  // (the point inside the body)
        const disc = qb * qb - 4 * qa * qc;
        if (disc < 0 || qa === 0) continue;
        const s = (-qb - Math.sqrt(disc)) / (2 * qa);       // (where the segment enters the body)
        if (s > 0 && s < 1) return true;
    }
    return false;
}

function placeLabels(cam) {
    const w = window.innerWidth, h = window.innerHeight;
    const shown = visibleBodies();
    eye.setFromMatrixPosition(cam.matrixWorld);
    for (const l of labels) {
        const visible = shownInScene(l.object);
        anchor.copy(l.offset).applyMatrix4(l.object.matrixWorld);
        projected.copy(anchor).project(cam);
        if (!visible || projected.z > 1 || projected.z < -1 || Math.abs(projected.x) > 1.2 || Math.abs(projected.y) > 1.2 ||
            (!l.onTop && hidden(anchor, shown))) {
            l.el.style.display = 'none';
            continue;
        }
        l.el.style.display = '';
        l.el.style.left = `${(0.5 + 0.5 * projected.x) * w}px`;
        l.el.style.top = `${(0.5 - 0.5 * projected.y) * h}px`;
    }
}

function resize() {
    renderer.setPixelRatio(window.devicePixelRatio || 1);
    renderer.setSize(window.innerWidth, window.innerHeight);
}
window.addEventListener('resize', resize);

let last = performance.now();
let frameCount = 0;
function frame(ms) {
    const dt = Math.min(0.25, Math.max(0, (ms - last) / 1000));
    last = ms;
    if (T.animate) {
        if (playing) {
            now += dt * (T.scale || 1) * speed;
            if (now > T.end) now = T.start + (now - T.end) % Math.max(1e-30, T.end - T.start);
            if (now < T.start) now = T.end - (T.start - now) % Math.max(1e-30, T.end - T.start);
        }
        if (slider && document.activeElement !== slider) slider.value = String(Math.round(10000 * (now - T.start) / (T.end - T.start)));
        if (readout) readout.textContent = formatTime(now);
    }
    applyTime();
    scene.updateMatrixWorld();
    const cam = placeCamera();
    renderer.getDrawingBufferSize(size);
    const ratio = renderer.getPixelRatio();
    for (const m of lineMaterials) {
        m.uniforms.resolution.value.copy(size);
        m.uniforms.width.value = m.userData.width * ratio;
    }
    for (const m of pointMaterials) {
        m.uniforms.pixelRatio.value = ratio;
        if (m.uniforms.clock) m.uniforms.clock.value = ms / 1500;
    }
    renderer.clear();
    if (S.sky) {        // the sky: the camera's orientation only
        skyCamera.matrixWorld.extractRotation(cam.matrixWorld);
        skyCamera.matrixWorldInverse.copy(skyCamera.matrixWorld).invert();
        skyCamera.fov = cam.isPerspectiveCamera ? cam.fov : 45;
        skyCamera.aspect = window.innerWidth / Math.max(1, window.innerHeight);
        skyCamera.updateProjectionMatrix();
        renderer.render(skyScene, skyCamera);
        renderer.clearDepth();
    }
    renderer.render(scene, cam);
    placeLabels(cam);
    frameCount++;
    if (frameCount > 2 && pending.size === 0) window.vkfReady = true;
    requestAnimationFrame(frame);
}
applyTime();
requestAnimationFrame(frame);

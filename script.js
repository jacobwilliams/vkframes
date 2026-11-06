// A drifting star field behind the hero, the gallery's lightbox, and sections
// fading in as they scroll into view.

(function starfield() {
  const canvas = document.getElementById('starfield');
  if (!canvas) return;
  const ctx = canvas.getContext('2d');
  const reduced = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  let stars = [];
  let width = 0, height = 0, ratio = 1;

  function resize() {
    ratio = window.devicePixelRatio || 1;
    width = canvas.clientWidth;
    height = canvas.clientHeight;
    canvas.width = width * ratio;
    canvas.height = height * ratio;
    ctx.setTransform(ratio, 0, 0, ratio, 0, 0);
    const n = Math.round(width * height / 4500);
    stars = Array.from({ length: n }, () => ({
      x: Math.random() * width,
      y: Math.random() * height,
      r: Math.random() * 1.2 + 0.2,
      a: Math.random() * 0.7 + 0.2,
      v: Math.random() * 0.06 + 0.01,
      tw: Math.random() * Math.PI * 2,
    }));
  }

  function draw(time) {
    ctx.clearRect(0, 0, width, height);
    for (const s of stars) {
      if (!reduced) {
        s.x -= s.v;
        if (s.x < 0) s.x += width;
      }
      const twinkle = reduced ? 1 : 0.75 + 0.25 * Math.sin(time / 900 + s.tw);
      ctx.globalAlpha = s.a * twinkle;
      ctx.fillStyle = '#dfe7ff';
      ctx.beginPath();
      ctx.arc(s.x, s.y, s.r, 0, Math.PI * 2);
      ctx.fill();
    }
    if (!reduced) requestAnimationFrame(draw);
  }

  window.addEventListener('resize', resize);
  resize();
  requestAnimationFrame(draw);
})();

(function lightbox() {
  const box = document.getElementById('lightbox');
  const img = box.querySelector('img');
  const caption = box.querySelector('p');
  document.querySelectorAll('.gallery img, .hero-shot img, .code-shot img').forEach((el) => {
    el.addEventListener('click', () => {
      img.src = el.src;
      img.alt = el.alt;
      const fig = el.closest('figure');
      const cap = fig && fig.querySelector('figcaption');
      caption.textContent = cap ? cap.textContent : el.alt;
      box.hidden = false;
    });
  });
  const close = () => { box.hidden = true; };
  box.addEventListener('click', close);
  document.addEventListener('keydown', (e) => { if (e.key === 'Escape') close(); });
})();

(function reveal() {
  const items = document.querySelectorAll('.card, .gallery figure, .stat-list li, .platforms li, .code-window');
  if (!('IntersectionObserver' in window)) return;
  items.forEach((el) => el.classList.add('reveal'));
  const observer = new IntersectionObserver((entries) => {
    for (const entry of entries) {
      if (entry.isIntersecting) {
        entry.target.classList.add('visible');
        observer.unobserve(entry.target);
      }
    }
  }, { rootMargin: '0px 0px -60px 0px' });
  items.forEach((el) => observer.observe(el));
})();

// The scene's diagram: a box's variable highlighted in the code beside it
// (on hover, keyboard focus or a tap), the other lines dimmed.
(function codeLinks() {
  const code = document.querySelector('.code-window code');
  const panel = document.querySelector('.code-window');
  const nodes = document.querySelectorAll('.node[data-var]');
  if (!code || !panel || !nodes.length) return;

  // one element per line, so each can be highlighted
  code.innerHTML = code.innerHTML.split('\n').map((html) => `<span class="line">${html}</span>`).join('');
  const lines = Array.from(code.querySelectorAll('.line')).map((el) => {
    const copy = el.cloneNode(true);
    copy.querySelectorAll('.c, .s').forEach((c) => c.remove());   // not in comments or strings
    return { el, text: copy.textContent };
  });

  // a node's color, at another opacity
  function tint(color, alpha) {
    const m = color.match(/rgba?\(([^)]+)\)/);
    if (!m) return color;
    const [r, g, b] = m[1].split(',').map((v) => v.trim());
    return `rgba(${r}, ${g}, ${b}, ${alpha})`;
  }

  let active = null;
  function show(node) {
    clear();
    active = node;
    const name = node.dataset.var;
    const word = new RegExp(`(^|[^A-Za-z0-9_])${name}(?![A-Za-z0-9_])`);
    const color = getComputedStyle(node).borderColor;
    panel.style.setProperty('--hl-bg', tint(color, 0.22));
    panel.style.setProperty('--hl-bar', tint(color, 1));
    node.style.setProperty('--node-ring', tint(color, 0.6));
    node.classList.add('active');
    lines.forEach((l) => l.el.classList.toggle('hl', word.test(l.text)));
    panel.classList.add('highlighting');
  }
  function clear() {
    if (active) active.classList.remove('active');
    active = null;
    lines.forEach((l) => l.el.classList.remove('hl'));
    panel.classList.remove('highlighting');
  }

  nodes.forEach((node) => {
    node.addEventListener('mouseenter', () => show(node));
    node.addEventListener('mouseleave', clear);
    node.addEventListener('focus', () => show(node));
    node.addEventListener('blur', clear);
    node.addEventListener('click', (e) => {          // touch: a tap toggles
      e.stopPropagation();
      if (active === node) clear(); else show(node);
    });
  });
  document.addEventListener('click', clear);
})();

// The small program's video: still (its first frame) for those who ask for
// less motion.
(function calmVideo() {
  const video = document.querySelector('.code-shot video');
  if (!video) return;
  if (window.matchMedia('(prefers-reduced-motion: reduce)').matches) {
    video.removeAttribute('autoplay');
    video.pause();
  }
})();

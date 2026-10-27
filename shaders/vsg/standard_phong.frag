#version 450

// vkframes' Phong shader: VSG's standard Phong fragment shader (VulkanSceneGraph 1.1.16,
// vsg::createPhongShaderSet(); MIT License, Copyright (c) 2018 Robert Osfield), with
// vkframes' changes for OpenFrames' lighting model, each marked "vkframes:"
// (see src/vsg/vkf_layer.cpp, "Lighting"). vkframes gives it to VSG's Phong
// shader set in place of VSG's own; VSG compiles its variants (#defines) at
// run time. When VSG is upgraded, compare this file with VSG's new shader.

#extension GL_ARB_separate_shader_objects : enable
#pragma import_defines (VSG_TEXTURECOORD_0, VSG_TEXTURECOORD_1, VSG_TEXTURECOORD_2, VSG_TEXTURECOORD_3, VSG_POINT_SPRITE, VSG_DIFFUSE_MAP, VSG_GREYSCALE_DIFFUSE_MAP, VSG_DETAIL_MAP, VSG_EMISSIVE_MAP, VSG_LIGHTMAP_MAP, VSG_NORMAL_MAP, VSG_SPECULAR_MAP, VSG_TWO_SIDED_LIGHTING, VSG_SHADOWS_PCSS, VSG_SHADOWS_SOFT, VSG_SHADOWS_HARD, SHADOWMAP_DEBUG, VSG_ALPHA_TEST, VKF_NIGHT_MAP, VKF_VIEW_LIGHTING, VKF_BODY)

// define by default for backwards compatibility
#define VSG_SHADOWS_HARD

#if defined(VSG_TEXTURECOORD_3)
    #define VSG_TEXCOORD_COUNT 4
#elif defined(VSG_TEXTURECOORD_2)
    #define VSG_TEXCOORD_COUNT 3
#elif defined(VSG_TEXTURECOORD_1)
    #define VSG_TEXCOORD_COUNT 2
#else
    #define VSG_TEXCOORD_COUNT 1
#endif

#define VIEW_DESCRIPTOR_SET 0
#define MATERIAL_DESCRIPTOR_SET 1

const float PI = radians(180);

#ifdef VSG_DIFFUSE_MAP
layout(set = MATERIAL_DESCRIPTOR_SET, binding = 0) uniform sampler2D diffuseMap;
#endif

#ifdef VSG_DETAIL_MAP
layout(set = MATERIAL_DESCRIPTOR_SET, binding = 1) uniform sampler2D detailMap;
#endif

#ifdef VSG_NORMAL_MAP
layout(set = MATERIAL_DESCRIPTOR_SET, binding = 2) uniform sampler2D normalMap;
#endif

#ifdef VSG_LIGHTMAP_MAP
layout(set = MATERIAL_DESCRIPTOR_SET, binding = 3) uniform sampler2D aoMap;
#endif

#ifdef VSG_EMISSIVE_MAP
layout(set = MATERIAL_DESCRIPTOR_SET, binding = 4) uniform sampler2D emissiveMap;
#endif

#ifdef VSG_SPECULAR_MAP
layout(set = MATERIAL_DESCRIPTOR_SET, binding = 5) uniform sampler2D specularMap;
#endif

layout(set = MATERIAL_DESCRIPTOR_SET, binding = 10) uniform MaterialData
{
    vec4 ambientColor;
    vec4 diffuseColor;
    vec4 specularColor;
    vec4 emissiveColor;
    float shininess;
    float alphaMask;
    float alphaMaskCutoff;
} material;

layout(set = MATERIAL_DESCRIPTOR_SET, binding = 11) uniform TexCoordIndices
{
    // indices into texCoord[] array for each texture type
    int diffuseMap;
    int detailMap;
    int normalMap;
    int aoMap;
    int emissiveMap;
    int specularMap;
    int mrMap;
} texCoordIndices;

layout(constant_id = 3) const int lightDataSize = 256;
layout(set = VIEW_DESCRIPTOR_SET, binding = 0) uniform LightData
{
    vec4 values[lightDataSize];
} lightData;

layout(location = 0) in vec3 eyePos;
layout(location = 1) in vec3 normalDir;
layout(location = 2) in vec4 vertexColor;
layout(location = 3) in vec3 viewDir;
layout(location = 4) in vec2 texCoord[VSG_TEXCOORD_COUNT];

layout(location = 0) out vec4 outColor;

// vkframes: the night texture (OpenFrames' setNightTextureMap), shown where the surface is lit weakly
#ifdef VKF_NIGHT_MAP
layout(set = MATERIAL_DESCRIPTOR_SET, binding = 9) uniform sampler2D nightMap;
#endif
// vkframes: a body's lighting (meshes made by vkf_mesh_create): x = the sine of its twilight angle, how
// far below the horizon (the Sun's elevation) its diffuse light reaches, fading out (0: none)
#ifdef VKF_BODY
layout(set = MATERIAL_DESCRIPTOR_SET, binding = 12) uniform VkfBody
{
    vec4 params;
} vkfBody;
#endif

// vkframes: the diffuse factor for a surface at cos(angle to the light) LdotN: Lambert's max(LdotN, 0),
// or with a twilight (sine s) extended past the terminator, (LdotN + s)^2 / 4s from -s to s, where it
// meets Lambert's smoothly (the same value and slope at s; never less light)
float vkf_diffuse(float LdotN)
{
#ifdef VKF_BODY
    float s = vkfBody.params.x;
    if (s > 0.0 && LdotN < s) return (LdotN > -s) ? (LdotN + s) * (LdotN + s) / (4.0 * s) : 0.0;
#endif
    return max(LdotN, 0.0);
}

vec3 vkf_lit = vec3(0.0);   // vkframes: the ambient and diffuse light received, in the lights' colors (for the night texture)

// include the calculateShadowCoverageForDirectionalLight(..) implementation
// Start of include code : shadows.glsl
layout(set = VIEW_DESCRIPTOR_SET, binding = 2) uniform texture2DArray shadowMaps;
#ifdef VSG_SHADOWS_PCSS
layout(set = VIEW_DESCRIPTOR_SET, binding = 3) uniform sampler shadowMapDirectSampler;
#endif
layout(set = VIEW_DESCRIPTOR_SET, binding = 4) uniform sampler shadowMapShadowSampler;

#if defined(VSG_SHADOWS_PCSS) || defined(VSG_SHADOWS_SOFT)
layout(constant_id = 0) const int shadowSamples = 16;

const int POISSON_DISK_SAMPLE_COUNT = 64;
const vec2 POISSON_DISK[POISSON_DISK_SAMPLE_COUNT] = {
    vec2(0.171472949063334, -0.8759142124127289),
    vec2(-0.1597672367362282, -0.9870644436411294),
    vec2(0.459697404042723, -0.8797412480505925),
    vec2(-0.3406895149639895, -0.82344072126091),
    vec2(-0.18011878164860493, -0.752673990614504),
    vec2(-0.01884770433660303, -0.7254192612116284),
    vec2(0.6144282076078238, -0.7121903439292749),
    vec2(0.1819601661258839, -0.6295125014796584),
    vec2(0.5696397140833599, -0.44800960234710685),
    vec2(-0.5311960973007043, -0.624279464282425),
    vec2(0.020650613816033614, -0.5581860968799359),
    vec2(0.38703123536143025, -0.5641755522846804),
    vec2(0.9052817101356537, -0.39936079009693787),
    vec2(0.7580135740654145, -0.4786856314890952),
    vec2(-0.30411976742779834, -0.48241538424801134),
    vec2(-0.505431959819757, -0.4276317626664276),
    vec2(-0.6600816830594087, -0.6331140935867211),
    vec2(-0.7975056676071671, -0.40957824210315424),
    vec2(0.2924778762580064, -0.3475145711718533),
    vec2(0.44252241482953325, -0.2659707461940645),
    vec2(-0.6241270408248075, -0.23094051769162596),
    vec2(-0.9138660377712776, -0.24042345091987666),
    vec2(-0.35570851106582574, -0.24762365106169779),
    vec2(0.9537344682210409, -0.13057946999488368),
    vec2(-0.006900019862990347, -0.25144628066897423),
    vec2(-0.698289978266092, -0.044637697647857646),
    vec2(0.5998885897645027, -0.23256852527042843),
    vec2(0.5582293140488868, 0.05634098985743327),
    vec2(-0.22782389126784444, -0.041355782331441264),
    vec2(-0.7026586581344971, 0.07399959917682604),
    vec2(0.3811626945604113, -0.05806040976666236),
    vec2(-0.1730576116608928, 0.11740348906220209),
    vec2(-0.5029332998560511, 0.1810152662195826),
    vec2(0.9806786102912116, 0.12146111532730393),
    vec2(0.023426166165222726, 0.013188008019334365),
    vec2(0.6705878741872812, 0.16439250606226838),
    vec2(0.7851830096686888, -0.01013595379709466),
    vec2(-0.07188359766368346, 0.20583196941854698),
    vec2(-0.8952731933727088, 0.2335154567298469),
    vec2(0.9060311492430866, 0.294333514916297),
    vec2(0.420534723772714, 0.6756751045566158),
    vec2(0.5386689661544318, 0.3389959292899256),
    vec2(-0.6730733987387932, 0.2931916967070102),
    vec2(-0.24810171005618564, 0.3605606959031977),
    vec2(-0.9048852880468572, 0.011094737605371743),
    vec2(0.6159910237674525, 0.47463589141275503),
    vec2(-0.07721288648155807, 0.39524667291002724),
    vec2(-0.8240173658848018, 0.5137812748207496),
    vec2(0.4364030788088093, 0.18255601185026235),
    vec2(0.19648517082664976, 0.5733772368850192),
    vec2(-0.364784578862518, 0.4992754241918426),
    vec2(-0.0603147280376663, 0.576476143664273),
    vec2(0.2267580025181592, 0.25817128520342786),
    vec2(-0.3982961540074632, 0.6813355551493548),
    vec2(0.7876024137377209, 0.5134471337683711),
    vec2(-0.22000277759700929, 0.7390659411823756),
    vec2(-0.26204354493233917, 0.88198927824126),
    vec2(0.1275517630407843, 0.7737872016994914),
    vec2(0.6428791055357954, 0.6889132748855609),
    vec2(0.463017393827676, 0.8217837822840931),
    vec2(-0.6630404728880878, 0.48627036913572896),
    vec2(-0.07705431457109976, 0.8326383985261969),
    vec2(0.11566549974066362, 0.9702036838357961),
    vec2(-0.42746287957561835, 0.8371864490608351),
};

// Interleaved Gradient Noise
// https://www.iryoku.com/next-generation-post-processing-in-call-of-duty-advanced-warfare
float quick_hash(vec2 pos) {
    const vec3 magic = vec3(0.06711056f, 0.00583715f, 52.9829189f);
    return fract(magic.z * fract(dot(pos, magic.xy)));
}
#endif


#ifdef VSG_SHADOWS_PCSS
// Start of include code : shadows_pcss.glsl
float calculateShadowCoverageForDirectionalLightPCSS(int lightDataIndex, int shadowMapIndex, vec3 T, vec3 B, inout vec3 color)
{
    vec4 shadowMapSettings = lightData.values[lightDataIndex++];
    int shadowMapCount = int(shadowMapSettings.r);
    int originalShadowMapIndex = shadowMapIndex;
    int originalIndex = lightDataIndex;

    const float blockerSearchRadius = shadowMapSettings.g;
    const float viableSampleRatio = 1;

    // Godot's implementation
    float diskRotation = quick_hash(gl_FragCoord.xy) * 2 * PI;
    mat2 diskRotationMatrix = mat2(cos(diskRotation), sin(diskRotation), -sin(diskRotation), cos(diskRotation));

    // blocker search
    bool matched = false;
    float overallBlockerDistances = 0;
    float overallBlockerCount = 0;
    int overallViableSamples = 0;
    while (shadowMapCount > 0 && !matched)
    {
        mat4 sm_matrix = mat4(lightData.values[lightDataIndex],
                              lightData.values[lightDataIndex+1],
                              lightData.values[lightDataIndex+2],
                              lightData.values[lightDataIndex+3]);
        float blockerDistances = 0.0;
        int blockerCount = 0;
        int viableSamples = 0;
        // always sample the original coordinates as otherwise blockers smaller than the search radius may be missed with small sample counts
        vec4 sm_tc = sm_matrix * vec4(eyePos, 1.0);
        sm_tc = vec4(sm_tc.xyz / sm_tc.w, 1.0);
        if (sm_tc.x >= 0.0 && sm_tc.x <= 1.0 && sm_tc.y >= 0.0 && sm_tc.y <= 1.0 && sm_tc.z >= 0.0 && sm_tc.z <= 1.0)
        {
            ++viableSamples;
            float blockerDistance = texture(sampler2DArray(shadowMaps, shadowMapDirectSampler), vec3(sm_tc.st, shadowMapIndex)).r;
            if (blockerDistance > sm_tc.z) {
                blockerDistances += blockerDistance;
                ++blockerCount;
            }
        }
        for (int i = 0; i < POISSON_DISK_SAMPLE_COUNT; i += POISSON_DISK_SAMPLE_COUNT / min(shadowSamples / 2, POISSON_DISK_SAMPLE_COUNT))
        {
            vec2 rotatedDisk = blockerSearchRadius * diskRotationMatrix * POISSON_DISK[i];
            sm_tc = sm_matrix * vec4(eyePos + rotatedDisk.x * T + rotatedDisk.y * B, 1.0);
            sm_tc = vec4(sm_tc.xyz / sm_tc.w, 1.0);
            if (sm_tc.x >= 0.0 && sm_tc.x <= 1.0 && sm_tc.y >= 0.0 && sm_tc.y <= 1.0 && sm_tc.z >= 0.0 && sm_tc.z <= 1.0)
            {
                ++viableSamples;
                float blockerDistance = texture(sampler2DArray(shadowMaps, shadowMapDirectSampler), vec3(sm_tc.st, shadowMapIndex)).r;
                if (blockerDistance > sm_tc.z) {
                    blockerDistances += blockerDistance;
                    ++blockerCount;
                }
            }
        }

        overallViableSamples += viableSamples;
        if (overallViableSamples >= viableSampleRatio * min(shadowSamples / 2, POISSON_DISK_SAMPLE_COUNT))
            matched = true;

        if (blockerCount > 0)
        {
            // if averaging like this is legal, then calculating the penumbra radius in light space should be legal, too
            blockerDistances /= blockerCount;

            mat4 sm_matrix_inv = mat4(lightData.values[lightDataIndex+4],
                                      lightData.values[lightDataIndex+5],
                                      lightData.values[lightDataIndex+6],
                                      lightData.values[lightDataIndex+7]);
            vec4 sm_tc = sm_matrix * vec4(eyePos, 1.0);
            sm_tc = vec4(sm_tc.xyz / sm_tc.w, 1.0);
            vec4 averageBlockerEuclidean = sm_matrix_inv * vec4(sm_tc.xy, blockerDistances, sm_tc.w);
            averageBlockerEuclidean.xyz /= averageBlockerEuclidean.w;
            float dist = distance(averageBlockerEuclidean.xyz, eyePos);

            overallBlockerCount += blockerCount;
            overallBlockerDistances = mix(overallBlockerDistances, dist, blockerCount / overallBlockerCount);
        }

        lightDataIndex += 8;
        ++shadowMapIndex;
        --shadowMapCount;
    }

    // there's something there, compute shadow
    // note - can't skip if all viable samples so far were blockers - if they're distant, the penumbra could be wider than the blocker search
    if (overallBlockerCount > 0)
    {
        float overallCoverage = 0;

        shadowMapCount = int(shadowMapSettings.r);
        shadowMapIndex = originalShadowMapIndex;
        lightDataIndex = originalIndex;

        float penumbraRadius = overallBlockerDistances * shadowMapSettings.b;

        float overallSampleCount = 0;
        while (shadowMapCount > 0)
        {
            mat4 sm_matrix = mat4(lightData.values[lightDataIndex],
                                  lightData.values[lightDataIndex+1],
                                  lightData.values[lightDataIndex+2],
                                  lightData.values[lightDataIndex+3]);

            float coverage = 0;
            int viableSamples = 0;
            for (int i = 0; i < POISSON_DISK_SAMPLE_COUNT; i += POISSON_DISK_SAMPLE_COUNT / min(shadowSamples, POISSON_DISK_SAMPLE_COUNT))
            {
                vec2 rotatedDisk = penumbraRadius * diskRotationMatrix * POISSON_DISK[i];
                vec4 sm_tc = sm_matrix * vec4(eyePos + rotatedDisk.x * T + rotatedDisk.y * B, 1.0);
                sm_tc = vec4(sm_tc.xyz / sm_tc.w, 1.0);
                if (sm_tc.x >= 0.0 && sm_tc.x <= 1.0 && sm_tc.y >= 0.0 && sm_tc.y <= 1.0 && sm_tc.z >= 0.0 && sm_tc.z <= 1.0)
                {
                    coverage += texture(sampler2DArrayShadow(shadowMaps, shadowMapShadowSampler), vec4(sm_tc.st, shadowMapIndex, sm_tc.z)).r;
                    ++viableSamples;
                }
            }

            coverage /= max(viableSamples, 1);
            overallSampleCount += viableSamples;
            overallCoverage = mix(overallCoverage, coverage, viableSamples / max(overallSampleCount, 1));

            if (overallSampleCount >= viableSampleRatio * min(shadowSamples, POISSON_DISK_SAMPLE_COUNT))
            {
                #ifdef SHADOWMAP_DEBUG
                            if (shadowMapIndex==0) color = vec3(1.0, 0.0, 0.0);
                            else if (shadowMapIndex==1) color = vec3(0.0, 1.0, 0.0);
                            else if (shadowMapIndex==2) color = vec3(0.0, 0.0, 1.0);
                            else if (shadowMapIndex==3) color = vec3(1.0, 1.0, 0.0);
                            else if (shadowMapIndex==4) color = vec3(0.0, 1.0, 1.0);
                            else color = vec3(1.0, 1.0, 1.0);
                #endif
                return overallCoverage;
            }

            lightDataIndex += 8;
            ++shadowMapIndex;
            --shadowMapCount;
        }
    }

    return 0.0;
}

float calculateShadowCoverageForSpotLightPCSS(int lightDataIndex, int shadowMapIndex, vec3 T, vec3 B, float lightDist, inout vec3 color)
{
    vec4 shadowMapSettings = lightData.values[lightDataIndex++];
    int shadowMapCount = int(shadowMapSettings.r);
    int originalShadowMapIndex = shadowMapIndex;
    int originalIndex = lightDataIndex;

    const float blockerSearchRadius = shadowMapSettings.g;
    const float viableSampleRatio = 1;

    // Godot's implementation
    float diskRotation = quick_hash(gl_FragCoord.xy) * 2 * PI;
    mat2 diskRotationMatrix = mat2(cos(diskRotation), sin(diskRotation), -sin(diskRotation), cos(diskRotation));

    // blocker search
    bool matched = false;
    float overallBlockerDistances = 0;
    float overallBlockerCount = 0;
    int overallViableSamples = 0;
    while (shadowMapCount > 0 && !matched)
    {
        mat4 sm_matrix = mat4(lightData.values[lightDataIndex],
                              lightData.values[lightDataIndex+1],
                              lightData.values[lightDataIndex+2],
                              lightData.values[lightDataIndex+3]);
        float blockerDistances = 0.0;
        int blockerCount = 0;
        int viableSamples = 0;
        // always sample the original coordinates as otherwise blockers smaller than the search radius may be missed with small sample counts
        vec4 sm_tc = sm_matrix * vec4(eyePos, 1.0);
        sm_tc = vec4(sm_tc.xyz / sm_tc.w, 1.0);
        if (sm_tc.x >= 0.0 && sm_tc.x <= 1.0 && sm_tc.y >= 0.0 && sm_tc.y <= 1.0 && sm_tc.z >= 0.0 && sm_tc.z <= 1.0)
        {
            ++viableSamples;
            float blockerDistance = texture(sampler2DArray(shadowMaps, shadowMapDirectSampler), vec3(sm_tc.st, shadowMapIndex)).r;
            if (blockerDistance > sm_tc.z) {
                blockerDistances += blockerDistance;
                ++blockerCount;
            }
        }
        for (int i = 0; i < POISSON_DISK_SAMPLE_COUNT; i += POISSON_DISK_SAMPLE_COUNT / min(shadowSamples / 2, POISSON_DISK_SAMPLE_COUNT))
        {
            vec2 rotatedDisk = blockerSearchRadius * diskRotationMatrix * POISSON_DISK[i];
            sm_tc = sm_matrix * vec4(eyePos + rotatedDisk.x * T + rotatedDisk.y * B, 1.0);
            sm_tc = vec4(sm_tc.xyz / sm_tc.w, 1.0);
            if (sm_tc.x >= 0.0 && sm_tc.x <= 1.0 && sm_tc.y >= 0.0 && sm_tc.y <= 1.0 && sm_tc.z >= 0.0 && sm_tc.z <= 1.0)
            {
                ++viableSamples;
                float blockerDistance = texture(sampler2DArray(shadowMaps, shadowMapDirectSampler), vec3(sm_tc.st, shadowMapIndex)).r;
                if (blockerDistance > sm_tc.z) {
                    blockerDistances += blockerDistance;
                    ++blockerCount;
                }
            }
        }

        overallViableSamples += viableSamples;
        if (overallViableSamples >= viableSampleRatio * min(shadowSamples / 2, POISSON_DISK_SAMPLE_COUNT))
            matched = true;

        if (blockerCount > 0)
        {
            // if averaging like this is legal, then calculating the penumbra radius in light space should be legal, too
            blockerDistances /= blockerCount;

            mat4 sm_matrix_inv = mat4(lightData.values[lightDataIndex+4],
                                      lightData.values[lightDataIndex+5],
                                      lightData.values[lightDataIndex+6],
                                      lightData.values[lightDataIndex+7]);
            vec4 sm_tc = sm_matrix * vec4(eyePos, 1.0);
            sm_tc = vec4(sm_tc.xyz / sm_tc.w, 1.0);
            vec4 averageBlockerEuclidean = sm_matrix_inv * vec4(sm_tc.xy, blockerDistances, sm_tc.w);
            averageBlockerEuclidean.xyz /= averageBlockerEuclidean.w;
            float dist = distance(averageBlockerEuclidean.xyz, eyePos);

            overallBlockerCount += blockerCount;
            overallBlockerDistances = mix(overallBlockerDistances, dist, blockerCount / overallBlockerCount);
        }

        lightDataIndex += 8;
        ++shadowMapIndex;
        --shadowMapCount;
    }

    // there's something there, compute shadow
    // note - can't skip if all viable samples so far were blockers - if they're distant, the penumbra could be wider than the blocker search
    if (overallBlockerCount > 0)
    {
        float overallCoverage = 0;

        shadowMapCount = int(shadowMapSettings.r);
        shadowMapIndex = originalShadowMapIndex;
        lightDataIndex = originalIndex;

        float penumbraRadius = overallBlockerDistances * shadowMapSettings.b / (lightDist - overallBlockerDistances);

        float overallSampleCount = 0;
        while (shadowMapCount > 0)
        {
            mat4 sm_matrix = mat4(lightData.values[lightDataIndex],
                                  lightData.values[lightDataIndex+1],
                                  lightData.values[lightDataIndex+2],
                                  lightData.values[lightDataIndex+3]);

            float coverage = 0;
            int viableSamples = 0;
            for (int i = 0; i < POISSON_DISK_SAMPLE_COUNT; i += POISSON_DISK_SAMPLE_COUNT / min(shadowSamples, POISSON_DISK_SAMPLE_COUNT))
            {
                vec2 rotatedDisk = penumbraRadius * diskRotationMatrix * POISSON_DISK[i];
                vec4 sm_tc = sm_matrix * vec4(eyePos + rotatedDisk.x * T + rotatedDisk.y * B, 1.0);
                sm_tc = vec4(sm_tc.xyz / sm_tc.w, 1.0);
                if (sm_tc.x >= 0.0 && sm_tc.x <= 1.0 && sm_tc.y >= 0.0 && sm_tc.y <= 1.0 && sm_tc.z >= 0.0 && sm_tc.z <= 1.0)
                {
                    coverage += texture(sampler2DArrayShadow(shadowMaps, shadowMapShadowSampler), vec4(sm_tc.st, shadowMapIndex, sm_tc.z)).r;
                    ++viableSamples;
                }
            }

            coverage /= max(viableSamples, 1);
            overallSampleCount += viableSamples;
            overallCoverage = mix(overallCoverage, coverage, viableSamples / max(overallSampleCount, 1));

            if (overallSampleCount >= viableSampleRatio * min(shadowSamples, POISSON_DISK_SAMPLE_COUNT))
            {
                #ifdef SHADOWMAP_DEBUG
                            if (shadowMapIndex==0) color = vec3(1.0, 0.0, 0.0);
                            else if (shadowMapIndex==1) color = vec3(0.0, 1.0, 0.0);
                            else if (shadowMapIndex==2) color = vec3(0.0, 0.0, 1.0);
                            else if (shadowMapIndex==3) color = vec3(1.0, 1.0, 0.0);
                            else if (shadowMapIndex==4) color = vec3(0.0, 1.0, 1.0);
                            else color = vec3(1.0, 1.0, 1.0);
                #endif
                return overallCoverage;
            }

            lightDataIndex += 8;
            ++shadowMapIndex;
            --shadowMapCount;
        }
    }

    return 0.0;
}
// End of include code : shadows_pcss.glsl

#endif

#ifdef VSG_SHADOWS_SOFT
// Start of include code : shadows_soft.glsl
float calculateShadowCoverageForDirectionalLightSoft(int lightDataIndex, int shadowMapIndex, vec3 T, vec3 B, inout vec3 color)
{
    vec4 shadowMapSettings = lightData.values[lightDataIndex++];
    int shadowMapCount = int(shadowMapSettings.r);

    const float viableSampleRatio = 1;

    // Godot's implementation
    float diskRotation = quick_hash(gl_FragCoord.xy) * 2 * PI;
    mat2 diskRotationMatrix = mat2(cos(diskRotation), sin(diskRotation), -sin(diskRotation), cos(diskRotation));

    float overallCoverage = 0;
    float overallSampleCount = 0;
    while (shadowMapCount > 0)
    {
        mat4 sm_matrix = mat4(lightData.values[lightDataIndex],
                              lightData.values[lightDataIndex+1],
                              lightData.values[lightDataIndex+2],
                              lightData.values[lightDataIndex+3]);

        float coverage = 0;
        int viableSamples = 0;
        for (int i = 0; i < POISSON_DISK_SAMPLE_COUNT; i += POISSON_DISK_SAMPLE_COUNT / min(shadowSamples, POISSON_DISK_SAMPLE_COUNT))
        {
            float penumbraRadius = shadowMapSettings.g;
            vec2 rotatedDisk = penumbraRadius * diskRotationMatrix * POISSON_DISK[i];
            vec4 sm_tc = sm_matrix * vec4(eyePos + rotatedDisk.x * T + rotatedDisk.y * B, 1.0);
            sm_tc = vec4(sm_tc.xyz / sm_tc.w, 1.0);
            if (sm_tc.x >= 0.0 && sm_tc.x <= 1.0 && sm_tc.y >= 0.0 && sm_tc.y <= 1.0 && sm_tc.z >= 0.0 && sm_tc.z <= 1.0)
            {
                coverage += texture(sampler2DArrayShadow(shadowMaps, shadowMapShadowSampler), vec4(sm_tc.st, shadowMapIndex, sm_tc.z)).r;
                ++viableSamples;
            }
        }

        coverage /= max(viableSamples, 1);
        overallSampleCount += viableSamples;
        overallCoverage = mix(overallCoverage, coverage, viableSamples / max(overallSampleCount, 1));

        if (overallSampleCount >= viableSampleRatio * min(shadowSamples, POISSON_DISK_SAMPLE_COUNT))
        {
        #ifdef SHADOWMAP_DEBUG
            if (shadowMapIndex==0) color = vec3(1.0, 0.0, 0.0);
            else if (shadowMapIndex==1) color = vec3(0.0, 1.0, 0.0);
            else if (shadowMapIndex==2) color = vec3(0.0, 0.0, 1.0);
            else if (shadowMapIndex==3) color = vec3(1.0, 1.0, 0.0);
            else if (shadowMapIndex==4) color = vec3(0.0, 1.0, 1.0);
            else color = vec3(1.0, 1.0, 1.0);
        #endif
            return overallCoverage;
        }

        lightDataIndex += 8;
        ++shadowMapIndex;
        --shadowMapCount;
    }

    return 0.0;
}

float calculateShadowCoverageForSpotLightSoft(int lightDataIndex, int shadowMapIndex, vec3 T, vec3 B, inout vec3 color)
{
    // identical to directional lights
    vec4 shadowMapSettings = lightData.values[lightDataIndex++];
    int shadowMapCount = int(shadowMapSettings.r);

    const float viableSampleRatio = 1;

    // Godot's implementation
    float diskRotation = quick_hash(gl_FragCoord.xy) * 2 * PI;
    mat2 diskRotationMatrix = mat2(cos(diskRotation), sin(diskRotation), -sin(diskRotation), cos(diskRotation));

    float overallCoverage = 0;
    float overallSampleCount = 0;
    while (shadowMapCount > 0)
    {
        mat4 sm_matrix = mat4(lightData.values[lightDataIndex],
                              lightData.values[lightDataIndex+1],
                              lightData.values[lightDataIndex+2],
                              lightData.values[lightDataIndex+3]);

        float coverage = 0;
        int viableSamples = 0;
        for (int i = 0; i < POISSON_DISK_SAMPLE_COUNT; i += POISSON_DISK_SAMPLE_COUNT / min(shadowSamples, POISSON_DISK_SAMPLE_COUNT))
        {
            float penumbraRadius = shadowMapSettings.g;
            vec2 rotatedDisk = penumbraRadius * diskRotationMatrix * POISSON_DISK[i];
            vec4 sm_tc = sm_matrix * vec4(eyePos + rotatedDisk.x * T + rotatedDisk.y * B, 1.0);
            sm_tc = vec4(sm_tc.xyz / sm_tc.w, 1.0);
            if (sm_tc.x >= 0.0 && sm_tc.x <= 1.0 && sm_tc.y >= 0.0 && sm_tc.y <= 1.0 && sm_tc.z >= 0.0 && sm_tc.z <= 1.0)
            {
                coverage += texture(sampler2DArrayShadow(shadowMaps, shadowMapShadowSampler), vec4(sm_tc.st, shadowMapIndex, sm_tc.z)).r;
                ++viableSamples;
            }
        }

        coverage /= max(viableSamples, 1);
        overallSampleCount += viableSamples;
        overallCoverage = mix(overallCoverage, coverage, viableSamples / max(overallSampleCount, 1));

        if (overallSampleCount >= viableSampleRatio * min(shadowSamples, POISSON_DISK_SAMPLE_COUNT))
        {
        #ifdef SHADOWMAP_DEBUG
            if (shadowMapIndex==0) color = vec3(1.0, 0.0, 0.0);
            else if (shadowMapIndex==1) color = vec3(0.0, 1.0, 0.0);
            else if (shadowMapIndex==2) color = vec3(0.0, 0.0, 1.0);
            else if (shadowMapIndex==3) color = vec3(1.0, 1.0, 0.0);
            else if (shadowMapIndex==4) color = vec3(0.0, 1.0, 1.0);
            else color = vec3(1.0, 1.0, 1.0);
        #endif
            return overallCoverage;
        }

        lightDataIndex += 8;
        ++shadowMapIndex;
        --shadowMapCount;
    }

    return 0.0;
}
// End of include code : shadows_soft.glsl

#endif

#ifdef VSG_SHADOWS_HARD
// Start of include code : shadows_hard.glsl
float calculateShadowCoverageForDirectionalLightHard(int lightDataIndex, int shadowMapIndex, inout vec3 color)
{
    vec4 shadowMapSettings = lightData.values[lightDataIndex++];
    int shadowMapCount = int(shadowMapSettings.r);

    while (shadowMapCount > 0)
    {
        float overallCoverage = 0;
        mat4 sm_matrix = mat4(lightData.values[lightDataIndex],
                              lightData.values[lightDataIndex+1],
                              lightData.values[lightDataIndex+2],
                              lightData.values[lightDataIndex+3]);

        vec4 sm_tc = sm_matrix * vec4(eyePos, 1.0);
        sm_tc = vec4(sm_tc.xyz / sm_tc.w, 1.0);

        if (sm_tc.x >= 0.0 && sm_tc.x <= 1.0 && sm_tc.y >= 0.0 && sm_tc.y <= 1.0 && sm_tc.z >= 0.0 && sm_tc.z <= 1.0)
        {
            overallCoverage = texture(sampler2DArrayShadow(shadowMaps, shadowMapShadowSampler), vec4(sm_tc.st, shadowMapIndex, sm_tc.z)).r;

#ifdef SHADOWMAP_DEBUG
            if (shadowMapIndex==0) color = vec3(1.0, 0.0, 0.0);
            else if (shadowMapIndex==1) color = vec3(0.0, 1.0, 0.0);
            else if (shadowMapIndex==2) color = vec3(0.0, 0.0, 1.0);
            else if (shadowMapIndex==3) color = vec3(1.0, 1.0, 0.0);
            else if (shadowMapIndex==4) color = vec3(0.0, 1.0, 1.0);
            else color = vec3(1.0, 1.0, 1.0);
#endif

            return overallCoverage;
        }

        lightDataIndex += 8;
        ++shadowMapIndex;
        --shadowMapCount;
    }

    return 0.0;
}

float calculateShadowCoverageForSpotLightHard(int lightDataIndex, int shadowMapIndex, inout vec3 color)
{
    // identical to directional lights
    vec4 shadowMapSettings = lightData.values[lightDataIndex++];
    int shadowMapCount = int(shadowMapSettings.r);

    while (shadowMapCount > 0)
    {
        float overallCoverage = 0;
        mat4 sm_matrix = mat4(lightData.values[lightDataIndex],
                              lightData.values[lightDataIndex+1],
                              lightData.values[lightDataIndex+2],
                              lightData.values[lightDataIndex+3]);

        vec4 sm_tc = sm_matrix * vec4(eyePos, 1.0);
        sm_tc = vec4(sm_tc.xyz / sm_tc.w, 1.0);

        if (sm_tc.x >= 0.0 && sm_tc.x <= 1.0 && sm_tc.y >= 0.0 && sm_tc.y <= 1.0 && sm_tc.z >= 0.0 && sm_tc.z <= 1.0)
        {
            overallCoverage = texture(sampler2DArrayShadow(shadowMaps, shadowMapShadowSampler), vec4(sm_tc.st, shadowMapIndex, sm_tc.z)).r;

#ifdef SHADOWMAP_DEBUG
            if (shadowMapIndex==0) color = vec3(1.0, 0.0, 0.0);
            else if (shadowMapIndex==1) color = vec3(0.0, 1.0, 0.0);
            else if (shadowMapIndex==2) color = vec3(0.0, 0.0, 1.0);
            else if (shadowMapIndex==3) color = vec3(1.0, 1.0, 0.0);
            else if (shadowMapIndex==4) color = vec3(0.0, 1.0, 1.0);
            else color = vec3(1.0, 1.0, 1.0);
#endif

            return overallCoverage;
        }

        lightDataIndex += 8;
        ++shadowMapIndex;
        --shadowMapCount;
    }

    return 0.0;
}
// End of include code : shadows_hard.glsl

#endif

float calculateShadowCoverageForDirectionalLight(int lightDataIndex, int shadowMapIndex, vec3 T, vec3 B, inout vec3 color)
{
    vec4 shadowMapSettings = lightData.values[lightDataIndex];
    int shadowMapCount = int(shadowMapSettings.r);
    if (shadowMapCount > 0)
    {
        if (shadowMapSettings.g < 0.0)
        {
#ifdef VSG_SHADOWS_HARD
            return calculateShadowCoverageForDirectionalLightHard(lightDataIndex, shadowMapIndex, color);
#else
            return 0;
#endif
        }
        else if (shadowMapSettings.b < 0.0)
        {
#ifdef VSG_SHADOWS_SOFT
            return calculateShadowCoverageForDirectionalLightSoft(lightDataIndex, shadowMapIndex, T, B, color);
#elif defined(VSG_SHADOWS_HARD)
            return calculateShadowCoverageForDirectionalLightHard(lightDataIndex, shadowMapIndex, color);
#else
            return 0;
#endif
        }
        else
        {
#ifdef VSG_SHADOWS_PCSS
            return calculateShadowCoverageForDirectionalLightPCSS(lightDataIndex, shadowMapIndex, T, B, color);
#elif defined(VSG_SHADOWS_HARD)
            return calculateShadowCoverageForDirectionalLightHard(lightDataIndex, shadowMapIndex, color);
#else
            return 0;
#endif
        }
    }
    return 0;
}

float calculateShadowCoverageForSpotLight(int lightDataIndex, int shadowMapIndex, vec3 T, vec3 B, float lightDist, inout vec3 color)
{
    vec4 shadowMapSettings = lightData.values[lightDataIndex];
    int shadowMapCount = int(shadowMapSettings.r);
    if (shadowMapCount > 0)
    {
        if (shadowMapSettings.g < 0.0)
        {
#ifdef VSG_SHADOWS_HARD
            return calculateShadowCoverageForSpotLightHard(lightDataIndex, shadowMapIndex, color);
#else
            return 0;
#endif
        }
        else if (shadowMapSettings.b < 0.0)
        {
#ifdef VSG_SHADOWS_SOFT
            return calculateShadowCoverageForSpotLightSoft(lightDataIndex, shadowMapIndex, T, B, color);
#elif defined(VSG_SHADOWS_HARD)
            return calculateShadowCoverageForSpotLightHard(lightDataIndex, shadowMapIndex, color);
#else
            return 0;
#endif
        }
        else
        {
#ifdef VSG_SHADOWS_PCSS
            return calculateShadowCoverageForSpotLightPCSS(lightDataIndex, shadowMapIndex, T, B, lightDist, color);
#elif defined(VSG_SHADOWS_HARD)
            return calculateShadowCoverageForSpotLightHard(lightDataIndex, shadowMapIndex, color);
#else
            return 0;
#endif
        }
    }
    return 0;
}
// End of include code : shadows.glsl


// Find the normal for this fragment, pulling either from a predefined normal map
// or from the interpolated mesh normal and tangent attributes.
vec3 getNormal()
{
    vec3 result;
#ifdef VSG_NORMAL_MAP
    // Perturb normal, see http://www.thetenthplanet.de/archives/1180
    vec3 tangentNormal = texture(normalMap, texCoord[texCoordIndices.normalMap]).xyz * 2.0 - 1.0;

    //tangentNormal *= vec3(2,2,1);

    vec3 q1 = dFdx(eyePos);
    vec3 q2 = dFdy(eyePos);
    vec2 st1 = dFdx(texCoord[texCoordIndices.normalMap]);
    vec2 st2 = dFdy(texCoord[texCoordIndices.normalMap]);

    vec3 N = normalize(normalDir);
    vec3 T = normalize(q1 * st2.t - q2 * st1.t);
    vec3 B = -normalize(cross(N, T));
    mat3 TBN = mat3(T, B, N);

    result = normalize(TBN * tangentNormal);
#else
    result = normalize(normalDir);
#endif
#ifdef VSG_TWO_SIDED_LIGHTING
    if (!gl_FrontFacing)
        result = -result;
#endif
    return result;
}

vec3 computeLighting(vec3 ambientColor, vec3 diffuseColor, vec3 specularColor, vec3 emissiveColor, float shininess, float ambientOcclusion, vec3 ld, vec3 nd, vec3 vd)
{
    vec3 color = vec3(0.0);
    color.rgb += ambientColor;

    float diff = max(dot(ld, nd), 0.0);
    color.rgb += diffuseColor * diff;

    if (diff > 0.0)
    {
        vec3 halfDir = normalize(ld + vd);
        color.rgb += specularColor * pow(max(dot(halfDir, nd), 0.0), shininess);
    }

    vec3 result = color + emissiveColor;
    result *= ambientOcclusion;

    return result;
}


void main()
{
    float intensityMinimum = 0.001;

#ifdef VSG_POINT_SPRITE
    const vec2 texCoordDiffuse = gl_PointCoord.xy;
#else
    const vec2 texCoordDiffuse = texCoord[texCoordIndices.diffuseMap].st;
#endif

    vec4 diffuseColor = vertexColor * material.diffuseColor;
#ifdef VSG_DIFFUSE_MAP
    #ifdef VSG_GREYSCALE_DIFFUSE_MAP
        float v = texture(diffuseMap, texCoordDiffuse).s;
        diffuseColor *= vec4(v, v, v, 1.0);
    #else
        diffuseColor *= texture(diffuseMap, texCoordDiffuse);
    #endif
#endif

#ifdef VSG_DETAIL_MAP
    vec4 detailColor = texture(detailMap, texCoord[texCoordIndices.detailMap].st);
    diffuseColor.rgb = mix(diffuseColor.rgb, detailColor.rgb, detailColor.a);
#endif

    vec4 ambientColor = diffuseColor * material.ambientColor * material.ambientColor.a;
    vec4 specularColor = material.specularColor;
    vec4 emissiveColor = material.emissiveColor;
    float shininess = material.shininess;
    float ambientOcclusion = 1.0;

#ifdef VSG_ALPHA_TEST
    if (material.alphaMask == 1.0f && diffuseColor.a < material.alphaMaskCutoff) discard;
#endif

#ifdef VSG_EMISSIVE_MAP
    emissiveColor *= texture(emissiveMap, texCoord[texCoordIndices.emissiveMap].st);
#endif

#ifdef VSG_LIGHTMAP_MAP
    ambientOcclusion *= texture(aoMap, texCoord[texCoordIndices.aoMap].st).r;
#endif

#ifdef VSG_SPECULAR_MAP
    specularColor *= texture(specularMap, texCoord[texCoordIndices.specularMap].st);
#endif

    vec3 nd = getNormal();
    vec3 vd = normalize(viewDir);

    vec3 color = vec3(0.0, 0.0, 0.0);

    vec4 lightNums = lightData.values[0];
    int numAmbientLights = int(lightNums[0]);
    int numDirectionalLights = int(lightNums[1]);
    int numPointLights = int(lightNums[2]);
    int numSpotLights = int(lightNums[3]);
    int lightDataIndex = 1;

    if (numAmbientLights>0)
    {
        // ambient lights
        for(int i = 0; i<numAmbientLights; ++i)
        {
            vec4 lightColor = lightData.values[lightDataIndex++];
            color += (ambientColor.rgb * lightColor.rgb) * (lightColor.a);
            vkf_lit += material.ambientColor.rgb * material.ambientColor.a * lightColor.rgb * lightColor.a;
        }
    }

    // index used to step through the shadowMaps array
    int shadowMapIndex = 0;

    if (numDirectionalLights>0)
    {
        vec3 q1 = dFdx(eyePos);
        vec3 q2 = dFdy(eyePos);
        vec2 st1 = dFdx(texCoord[0]);
        vec2 st2 = dFdy(texCoord[0]);

        vec3 N = normalize(normalDir);
        vec3 T = normalize(q1 * st2.t - q2 * st1.t);
        vec3 B = -normalize(cross(N, T));

        // directional lights
        for(int i = 0; i<numDirectionalLights; ++i)
        {
            vec4 lightColor = lightData.values[lightDataIndex++];
            // vkframes: a light's specular part has a negative color (lights are diffuse or specular)
            bool vkf_specular = any(lessThan(lightColor.rgb, vec3(0.0)));
            if (vkf_specular) lightColor.rgb = -lightColor.rgb;
            vec3 direction = -lightData.values[lightDataIndex++].xyz;

            float intensity = lightColor.a;

            float unclamped_LdotN = dot(direction, nd);

            float diff = vkf_diffuse(unclamped_LdotN);   // vkframes: with the body's twilight
            intensity *= diff;

            int shadowMapCount = int(lightData.values[lightDataIndex].r);
            if (shadowMapCount > 0)
            {
                if (intensity > intensityMinimum)
                    intensity *= (1.0-calculateShadowCoverageForDirectionalLight(lightDataIndex, shadowMapIndex, T, B, color));

                lightDataIndex += 1 + 8 * shadowMapCount;
                shadowMapIndex += shadowMapCount;
            }
            else
                lightDataIndex++;

            // if light is too shadowed to effect the rendering skip it
            if (intensity < intensityMinimum ) continue;

            if (!vkf_specular)   // vkframes: diffuse light only (and how much, for the night texture)
            {
                color.rgb += (diffuseColor.rgb * lightColor.rgb) * (intensity);
                vkf_lit += lightColor.rgb * intensity;
            }

            if (vkf_specular && shininess > 0.0 && unclamped_LdotN > 0.0)   // vkframes: specular light only
            {
                vec3 halfDir = normalize(direction + vd);
                color.rgb += lightColor.rgb * specularColor.rgb * (   // vkframes: in the light's specular color
                    pow(max(dot(halfDir, nd), 0.0), shininess) * intensity);
            }
        }
    }

    if (numPointLights>0)
    {
        // point light
        for(int i = 0; i<numPointLights; ++i)
        {
            vec4 lightColor = lightData.values[lightDataIndex++];
            // vkframes: a light's specular part has a negative color (lights are diffuse or specular)
            bool vkf_specular = any(lessThan(lightColor.rgb, vec3(0.0)));
            if (vkf_specular) lightColor.rgb = -lightColor.rgb;
            vec3 position = lightData.values[lightDataIndex++].xyz;

            float intensity = lightColor.a;

            // if light is too dim/shadowed to effect the rendering skip it
            if (intensity < intensityMinimum ) continue;

            vec3 delta = position - eyePos;
            float distance2 = delta.x * delta.x + delta.y * delta.y + delta.z * delta.z;
            vec3 direction = delta / sqrt(distance2);
            float scale = intensity;   // vkframes: no falloff with distance (OpenGL's default, as OpenFrames)

            float unclamped_LdotN = dot(direction, nd);

            float diff = scale * vkf_diffuse(unclamped_LdotN);   // vkframes: with the body's twilight

            if (!vkf_specular)   // vkframes: diffuse light only (and how much, for the night texture)
            {
                color.rgb += (diffuseColor.rgb * lightColor.rgb) * diff;
                vkf_lit += lightColor.rgb * diff;
            }
            if (vkf_specular && shininess > 0.0 && unclamped_LdotN > 0.0)   // vkframes: specular light only
            {
                vec3 halfDir = normalize(direction + vd);
                color.rgb += lightColor.rgb * specularColor.rgb * (   // vkframes: in the light's specular color
                    pow(max(dot(halfDir, nd), 0.0), shininess) * scale);
            }
        }
    }

    if (numSpotLights>0)
    {
        vec3 q1 = dFdx(eyePos);
        vec3 q2 = dFdy(eyePos);
        vec2 st1 = dFdx(texCoord[0]);
        vec2 st2 = dFdy(texCoord[0]);

        vec3 N = normalize(normalDir);
        vec3 T = normalize(q1 * st2.t - q2 * st1.t);
        vec3 B = -normalize(cross(N, T));

        // spot light
        for(int i = 0; i<numSpotLights; ++i)
        {
            vec4 lightColor = lightData.values[lightDataIndex++];
            // vkframes: a light's specular part has a negative color (lights are diffuse or specular)
            bool vkf_specular = any(lessThan(lightColor.rgb, vec3(0.0)));
            if (vkf_specular) lightColor.rgb = -lightColor.rgb;
            vec4 position_cosInnerAngle = lightData.values[lightDataIndex++];
            vec4 lightDirection_cosOuterAngle = lightData.values[lightDataIndex++];

            float intensity = lightColor.a;

            vec3 delta = position_cosInnerAngle.xyz - eyePos;
            float distance2 = delta.x * delta.x + delta.y * delta.y + delta.z * delta.z;
            float dist = sqrt(distance2);

            vec3 direction = delta / dist;

            float dot_lightdirection = dot(lightDirection_cosOuterAngle.xyz, -direction);

            int shadowMapCount = int(lightData.values[lightDataIndex].r);
            if (shadowMapCount > 0)
            {
                if (lightDirection_cosOuterAngle.w < dot_lightdirection)
                    intensity *= (1.0-calculateShadowCoverageForSpotLight(lightDataIndex, shadowMapIndex, T, B, dist, color));

                lightDataIndex += 1 + 8 * shadowMapCount;
                shadowMapIndex += shadowMapCount;
            }
            else
                lightDataIndex++;

            // if light is too shadowed to effect the rendering skip it
            if (intensity < intensityMinimum ) continue;

            float scale = (intensity * smoothstep(lightDirection_cosOuterAngle.w, position_cosInnerAngle.w, dot_lightdirection)) / distance2;

            float unclamped_LdotN = dot(direction, nd);

            float diff = scale * vkf_diffuse(unclamped_LdotN);   // vkframes: with the body's twilight
            if (!vkf_specular)   // vkframes: diffuse light only (and how much, for the night texture)
            {
                color.rgb += (diffuseColor.rgb * lightColor.rgb) * diff;
                vkf_lit += lightColor.rgb * diff;
            }
            if (vkf_specular && shininess > 0.0 && unclamped_LdotN > 0.0)   // vkframes: specular light only
            {
                vec3 halfDir = normalize(direction + vd);
                color.rgb += lightColor.rgb * specularColor.rgb * (   // vkframes: in the light's specular color
                    pow(max(dot(halfDir, nd), 0.0), shininess) * scale);
            }
        }
    }

    outColor.rgb = (color * ambientOcclusion) + emissiveColor.rgb;
#ifdef VKF_NIGHT_MAP
    // vkframes: the night texture where the surface is lit weakly, as OpenFrames' (its texture combiner
    // weighs the night texture by 1 - the lit color), per pixel: the lit color without the textures (the
    // material's ambient and diffuse colors in the lights' colors and intensities, and its emission), per
    // channel, so a brighter light narrows the twilight band and ambient light fades the night texture
    vec3 vkf_lit_color = (vertexColor.rgb * material.diffuseColor.rgb) * vkf_lit * ambientOcclusion + emissiveColor.rgb;
    outColor.rgb += texture(nightMap, texCoordDiffuse).rgb * (1.0 - clamp(vkf_lit_color, 0.0, 1.0));
#endif
#ifdef VKF_VIEW_LIGHTING
    // vkframes: view lighting (frames' axes): shaded by how the surface faces the viewer, not by the lights
    outColor.rgb = diffuseColor.rgb * (0.55 + 0.45 * abs(dot(nd, vd))) + emissiveColor.rgb;
#endif
    outColor.a = diffuseColor.a;
}

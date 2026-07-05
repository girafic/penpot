;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.colorpicker.shader-presets
  "Built-in SkSL shader presets for the shader fill colorpicker tab.

  All presets follow the shader fill uniform contract: the optional
  uniforms `u_time` (seconds), `u_resolution` (shape size in px),
  `u_color1..4` (RGBA) and `u_param1..4` (floats) are filled by the
  wasm renderer; `fragCoord` is local to the shape (origin at the
  top-left corner of its selection rect). Declaring `u_time` makes the
  shader animated.")

(def default-source
  "uniform float2 u_resolution;

half4 main(float2 fragCoord) {
  float2 uv = fragCoord / u_resolution;
  return half4(float4(uv.x, uv.y, 0.5, 1.0));
}")

(def ^:private noise-gradient-source
  "uniform float u_time;
uniform float2 u_resolution;
uniform float4 u_color1;
uniform float4 u_color2;

float hash(float2 p) {
  return fract(sin(dot(p, float2(127.1, 311.7))) * 43758.5453123);
}

float noise(float2 p) {
  float2 i = floor(p);
  float2 f = fract(p);
  float2 u = f * f * (3.0 - 2.0 * f);
  return mix(mix(hash(i), hash(i + float2(1.0, 0.0)), u.x),
             mix(hash(i + float2(0.0, 1.0)), hash(i + float2(1.0, 1.0)), u.x),
             u.y);
}

half4 main(float2 fragCoord) {
  float2 uv = fragCoord / u_resolution;
  float n = noise(uv * 3.0 + u_time * 0.2);
  n = n * 0.7 + 0.3 * noise(uv * 9.0 - u_time * 0.1);
  return half4(mix(u_color1, u_color2, n));
}")

(def ^:private waves-source
  "uniform float u_time;
uniform float2 u_resolution;
uniform float4 u_color1;
uniform float4 u_color2;
uniform float u_param1; // frequency
uniform float u_param2; // speed

half4 main(float2 fragCoord) {
  float2 uv = fragCoord / u_resolution;
  float freq = max(u_param1, 0.1) * 6.2831;
  float t = u_time * u_param2;
  float w = 0.5 * sin(uv.x * freq + t)
          + 0.25 * sin(uv.x * freq * 1.7 - t * 1.3 + uv.y * 2.0);
  float line = smoothstep(0.35, 0.0, abs(uv.y - 0.5 - w * 0.15));
  return half4(mix(u_color2, u_color1, line));
}")

(def ^:private plasma-source
  "uniform float u_time;
uniform float2 u_resolution;

half4 main(float2 fragCoord) {
  float2 uv = fragCoord / u_resolution * 6.2831;
  float t = u_time * 0.7;
  float v = sin(uv.x + t)
          + sin(uv.y + t * 0.8)
          + sin(uv.x + uv.y + t)
          + sin(length(uv - 3.1415) + t * 1.2);
  float3 col = 0.5 + 0.5 * cos(float3(v, v + 2.094, v + 4.188));
  return half4(float4(col, 1.0));
}")

(def ^:private mesh-gradient-source
  "uniform float2 u_resolution;
uniform float4 u_color1;
uniform float4 u_color2;
uniform float4 u_color3;
uniform float4 u_color4;

half4 main(float2 fragCoord) {
  float2 uv = fragCoord / u_resolution;
  float4 a = mix(u_color1, u_color2, uv.x);
  float4 b = mix(u_color3, u_color4, uv.x);
  return half4(mix(a, b, uv.y));
}")

(def presets
  "Ordered collection of the built-in shader presets. The `:name` is
  stored in the fill data (`:preset`) so it can be displayed later."
  [{:name "noise-gradient"
    :source noise-gradient-source
    :colors ["#7c3aed" "#4ade80"]
    :params []}
   {:name "waves"
    :source waves-source
    :colors ["#0ea5e9" "#0f172a"]
    :params [1 1]}
   {:name "plasma"
    :source plasma-source
    :colors []
    :params []}
   {:name "mesh-gradient"
    :source mesh-gradient-source
    :colors ["#7c3aed" "#0ea5e9" "#f472b6" "#facc15"]
    :params []}])

(defn find-preset
  [name]
  (or (first (filter #(= (:name %) name) presets))
      (first presets)))

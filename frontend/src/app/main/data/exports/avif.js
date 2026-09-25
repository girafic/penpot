/**
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) KALEIDOS INC
 */

// Writer for animated AVIF: an AVIF image sequence (brand "avis") made of
// the AV1 temporal units a WebCodecs VideoEncoder outputs. The first frame
// is also the primary still image, so readers without sequence support
// show it. Box layout follows libavif; see the AV1 ISOBMFF binding
// (av1-isobmff), AVIF 1.1 and ISO/IEC 14496-12.

const OBU_SEQUENCE_HEADER = 1;
const OBU_TEMPORAL_DELIMITER = 2;

// --- AV1 bitstream

function readLeb128(bytes, pos) {
  let value = 0;
  for (let i = 0; i < 8; i++) {
    const byte = bytes[pos + i];
    if (byte === undefined) {
      return null;
    }
    value += (byte & 0x7f) * 2 ** (i * 7);
    if (!(byte & 0x80)) {
      return { value, length: i + 1 };
    }
  }
  return null;
}

/**
 * The OBUs of a temporal unit as `{type, start, end, payloadStart}`, or
 * null when the bytes are not a whole sequence of OBUs.
 */
export function obus(bytes) {
  const result = [];
  let pos = 0;
  while (pos < bytes.length) {
    const header = bytes[pos];
    const type = (header >> 3) & 0x0f;
    const hasExtension = (header >> 2) & 1;
    const hasSize = (header >> 1) & 1;
    let payloadStart = pos + 1 + hasExtension;
    let end = bytes.length;
    if (hasSize) {
      const size = readLeb128(bytes, payloadStart);
      if (size === null) {
        return null;
      }
      payloadStart += size.length;
      end = payloadStart + size.value;
    }
    if (end > bytes.length) {
      return null;
    }
    result.push({ type, start: pos, end, payloadStart });
    pos = end;
  }
  return result;
}

/**
 * `bytes` without temporal delimiter OBUs, which ISOBMFF samples must not
 * carry. Bytes that do not parse are returned unchanged.
 */
export function withoutTemporalDelimiters(bytes) {
  const all = obus(bytes);
  if (all === null) {
    return bytes;
  }
  const kept = all.filter((obu) => obu.type !== OBU_TEMPORAL_DELIMITER);
  if (kept.length === all.length) {
    return bytes;
  }
  const out = new Uint8Array(kept.reduce((n, o) => n + o.end - o.start, 0));
  let pos = 0;
  for (const obu of kept) {
    out.set(bytes.subarray(obu.start, obu.end), pos);
    pos += obu.end - obu.start;
  }
  return out;
}

/** The first sequence header OBU (header included) of `bytes`, or null. */
export function sequenceHeaderObu(bytes) {
  const obu = (obus(bytes) || []).find((o) => o.type === OBU_SEQUENCE_HEADER);
  return obu ? bytes.subarray(obu.start, obu.end) : null;
}

class BitReader {
  constructor(bytes) {
    this.bytes = bytes;
    this.pos = 0;
  }

  read(n) {
    let value = 0;
    for (let i = 0; i < n; i++) {
      const byte = this.bytes[this.pos >> 3] ?? 0;
      value = value * 2 + ((byte >> (7 - (this.pos & 7))) & 1);
      this.pos++;
    }
    return value;
  }

  uvlc() {
    let zeros = 0;
    while (zeros < 32 && !this.read(1)) {
      zeros++;
    }
    return zeros >= 32 ? 2 ** 32 - 1 : this.read(zeros) + 2 ** zeros - 1;
  }
}

/**
 * The fields of an AV1 sequence header OBU payload that the AV1 codec
 * configuration record repeats (AV1 spec, 5.5).
 */
export function parseSequenceHeader(payload) {
  const b = new BitReader(payload);
  const profile = b.read(3);
  b.read(1); // still_picture
  const reduced = b.read(1);
  let level = 0;
  let tier = 0;

  if (reduced) {
    level = b.read(5);
  } else {
    let decoderModelInfo = 0;
    let bufferDelayLength = 0;
    if (b.read(1)) {
      // timing_info
      b.read(32);
      b.read(32);
      if (b.read(1)) {
        b.uvlc();
      }
      decoderModelInfo = b.read(1);
      if (decoderModelInfo) {
        bufferDelayLength = b.read(5) + 1;
        b.read(32);
        b.read(5);
        b.read(5);
      }
    }
    const initialDisplayDelay = b.read(1);
    const operatingPoints = b.read(5) + 1;
    for (let i = 0; i < operatingPoints; i++) {
      b.read(12); // operating_point_idc
      const opLevel = b.read(5);
      const opTier = opLevel > 7 ? b.read(1) : 0;
      if (i === 0) {
        level = opLevel;
        tier = opTier;
      }
      if (decoderModelInfo && b.read(1)) {
        b.read(bufferDelayLength);
        b.read(bufferDelayLength);
        b.read(1);
      }
      if (initialDisplayDelay && b.read(1)) {
        b.read(4);
      }
    }
  }

  const widthBits = b.read(4) + 1;
  const heightBits = b.read(4) + 1;
  b.read(widthBits);
  b.read(heightBits);
  if (!reduced && b.read(1)) {
    // frame_id_numbers_present_flag
    b.read(4);
    b.read(3);
  }
  b.read(3); // use_128x128_superblock, enable_filter_intra, enable_intra_edge_filter
  if (!reduced) {
    b.read(4); // interintra, masked compound, warped motion, dual filter
    const orderHint = b.read(1);
    if (orderHint) {
      b.read(2); // enable_jnt_comp, enable_ref_frame_mvs
    }
    const screenContentTools = b.read(1) ? 2 : b.read(1);
    if (screenContentTools > 0 && !b.read(1)) {
      b.read(1); // seq_force_integer_mv
    }
    if (orderHint) {
      b.read(3);
    }
  }
  b.read(3); // enable_superres, enable_cdef, enable_restoration

  // color_config
  const highBitdepth = b.read(1);
  const twelveBit = profile === 2 && highBitdepth ? b.read(1) : 0;
  const bitDepth = twelveBit ? 12 : highBitdepth ? 10 : 8;
  const monochrome = profile === 1 ? 0 : b.read(1);
  let primaries = 2;
  let transfer = 2;
  let matrix = 2;
  if (b.read(1)) {
    primaries = b.read(8);
    transfer = b.read(8);
    matrix = b.read(8);
  }
  let subsamplingX = 1;
  let subsamplingY = 1;
  let chromaSamplePosition = 0;
  let fullRange;
  if (monochrome) {
    fullRange = b.read(1);
  } else if (primaries === 1 && transfer === 13 && matrix === 0) {
    fullRange = 1;
    subsamplingX = 0;
    subsamplingY = 0;
  } else {
    fullRange = b.read(1);
    if (profile === 1) {
      subsamplingX = 0;
      subsamplingY = 0;
    } else if (profile === 2) {
      if (bitDepth === 12) {
        subsamplingX = b.read(1);
        subsamplingY = subsamplingX ? b.read(1) : 0;
      } else {
        subsamplingY = 0;
      }
    }
    if (subsamplingX && subsamplingY) {
      chromaSamplePosition = b.read(2);
    }
  }

  return {
    profile,
    level,
    tier,
    bitDepth,
    highBitdepth,
    twelveBit,
    monochrome,
    subsamplingX,
    subsamplingY,
    chromaSamplePosition,
    primaries,
    transfer,
    matrix,
    fullRange,
  };
}

// --- ISOBMFF boxes

const textEncoder = new TextEncoder();

function concat(parts) {
  const out = new Uint8Array(parts.reduce((n, p) => n + p.length, 0));
  let pos = 0;
  for (const part of parts) {
    out.set(part, pos);
    pos += part.length;
  }
  return out;
}

function u8(v) {
  return Uint8Array.of(v & 0xff);
}

function u16(v) {
  return Uint8Array.of((v >>> 8) & 0xff, v & 0xff);
}

function u32(v) {
  const out = new Uint8Array(4);
  new DataView(out.buffer).setUint32(0, v >>> 0);
  return out;
}

function u64(v) {
  const out = new Uint8Array(8);
  const view = new DataView(out.buffer);
  view.setUint32(0, Math.floor(v / 2 ** 32));
  view.setUint32(4, v >>> 0);
  return out;
}

// A duration of all ones: the track repeats forever.
const INDEFINITE = new Uint8Array(8).fill(0xff);

function fourcc(s) {
  return textEncoder.encode(s);
}

function box(type, ...parts) {
  const body = concat(parts);
  return concat([u32(8 + body.length), fourcc(type), body]);
}

function fullBox(type, version, flags, ...parts) {
  return box(type, u8(version), u8(flags >> 16), u8(flags >> 8), u8(flags), ...parts);
}

const UNITY_MATRIX = [0x00010000, 0, 0, 0, 0x00010000, 0, 0, 0, 0x40000000].map(u32);

function handler(type) {
  return fullBox("hdlr", 0, 0, u32(0), fourcc(type), u32(0), u32(0), u32(0), u8(0));
}

function codecConfiguration(sequenceHeader) {
  const all = obus(sequenceHeader);
  const info = parseSequenceHeader(sequenceHeader.subarray(all[0].payloadStart));
  const record = concat([
    u8(0x81), // marker, version 1
    u8((info.profile << 5) | info.level),
    u8(
      (info.tier << 7) |
        (info.highBitdepth << 6) |
        (info.twelveBit << 5) |
        (info.monochrome << 4) |
        (info.subsamplingX << 3) |
        (info.subsamplingY << 2) |
        info.chromaSamplePosition,
    ),
    u8(0),
    sequenceHeader,
  ]);
  return { info, av1C: box("av1C", record) };
}

function compressorName(name) {
  const out = new Uint8Array(32);
  out[0] = name.length;
  out.set(textEncoder.encode(name), 1);
  return out;
}

/**
 * Build an animated AVIF file from AV1 temporal units.
 *
 * - `width`, `height`: size of the coded frames.
 * - `timescale`: ticks per second of the sample durations.
 * - `samples`: `{data, key, duration}` in presentation order; `data` is a
 *   temporal unit, `key` marks key frames and `duration` is in ticks. The
 *   first sample must be a key frame with the sequence header.
 * - `loop`: repeat forever instead of playing once.
 * - `color`: optional CICP `{primaries, transfer, matrix, fullRange}` of
 *   the frames, stored as an nclx `colr` property.
 *
 * Returns a Uint8Array.
 */
export function writeSequence({ width, height, timescale, samples, loop, color }) {
  if (!samples || samples.length === 0) {
    throw new Error("An AVIF sequence needs at least one sample");
  }
  const units = samples.map((s) => ({
    data: withoutTemporalDelimiters(s.data),
    key: !!s.key,
    duration: s.duration,
  }));
  const sequenceHeader = sequenceHeaderObu(units[0].data);
  if (!units[0].key || sequenceHeader === null) {
    throw new Error("The first AVIF sample must be a key frame with a sequence header");
  }

  const { info, av1C } = codecConfiguration(sequenceHeader);
  const channels = info.monochrome ? 1 : 3;
  const pixi = fullBox("pixi", 0, 0, u8(channels), ...Array(channels).fill(u8(info.bitDepth)));
  const colr = color
    ? box(
        "colr",
        fourcc("nclx"),
        u16(color.primaries),
        u16(color.transfer),
        u16(color.matrix),
        u8(color.fullRange ? 0x80 : 0),
      )
    : null;
  const optional = colr ? [colr] : [];

  const sizes = units.map((s) => s.data.length);
  const total = units.reduce((n, s) => n + s.duration, 0);

  const ftyp = box(
    "ftyp",
    fourcc("avis"),
    u32(0),
    fourcc("avif"),
    fourcc("avis"),
    fourcc("msf1"),
    fourcc("iso8"),
    fourcc("mif1"),
    fourcc("miaf"),
  );

  // The primary item is the first sample, stored once in the mdat.
  const meta = (dataStart) =>
    fullBox(
      "meta",
      0,
      0,
      handler("pict"),
      fullBox("pitm", 0, 0, u16(1)),
      fullBox("iloc", 0, 0, u8(0x44), u8(0), u16(1), u16(1), u16(0), u16(1), u32(dataStart), u32(sizes[0])),
      fullBox("iinf", 0, 0, u16(1), fullBox("infe", 2, 0, u16(1), u16(0), fourcc("av01"), fourcc("Color"), u8(0))),
      box(
        "iprp",
        box("ipco", av1C, fullBox("ispe", 0, 0, u32(width), u32(height)), pixi, ...optional),
        fullBox("ipma", 0, 0, u32(1), u16(1), u8(3 + optional.length), u8(0x81), u8(0x02), u8(0x03), ...(colr ? [u8(0x04)] : [])),
      ),
    );

  const trackDuration = loop ? INDEFINITE : u64(total);

  const durations = [];
  for (const unit of units) {
    const last = durations[durations.length - 1];
    if (last && last.delta === unit.duration) {
      last.count++;
    } else {
      durations.push({ count: 1, delta: unit.duration });
    }
  }
  const syncSamples = units.flatMap((s, i) => (s.key ? [i + 1] : []));

  const sampleEntry = box(
    "av01",
    new Uint8Array(6),
    u16(1), // data_reference_index
    new Uint8Array(16),
    u16(width),
    u16(height),
    u32(0x00480000),
    u32(0x00480000),
    u32(0),
    u16(1), // frame_count
    compressorName("AOM Coding"),
    u16(0x0018),
    u16(0xffff),
    av1C,
    // ccst: intra prediction used, up to 15 references per picture
    fullBox("ccst", 0, 0, u32(0x7c000000)),
    ...optional,
  );

  const moov = (dataStart) =>
    box(
      "moov",
      fullBox(
        "mvhd",
        1,
        0,
        u64(0),
        u64(0),
        u32(timescale),
        trackDuration,
        u32(0x00010000),
        u16(0x0100),
        new Uint8Array(10),
        ...UNITY_MATRIX,
        new Uint8Array(24),
        u32(2), // next_track_ID
      ),
      box(
        "trak",
        fullBox(
          "tkhd",
          1,
          1, // track enabled
          u64(0),
          u64(0),
          u32(1), // track_ID
          u32(0),
          trackDuration,
          new Uint8Array(16),
          ...UNITY_MATRIX,
          u32(width * 0x10000),
          u32(height * 0x10000),
        ),
        // One edit covering all samples; flag 1 repeats it.
        box("edts", fullBox("elst", 1, loop ? 1 : 0, u32(1), u64(total), u64(0), u16(1), u16(0))),
        box(
          "mdia",
          fullBox("mdhd", 1, 0, u64(0), u64(0), u32(timescale), u64(total), u16(0x55c4), u16(0)),
          handler("pict"),
          box(
            "minf",
            fullBox("vmhd", 0, 1, new Uint8Array(8)),
            box("dinf", fullBox("dref", 0, 0, u32(1), fullBox("url ", 0, 1))),
            box(
              "stbl",
              fullBox("stsd", 0, 0, u32(1), sampleEntry),
              fullBox("stts", 0, 0, u32(durations.length), ...durations.flatMap((d) => [u32(d.count), u32(d.delta)])),
              ...(syncSamples.length === units.length
                ? []
                : [fullBox("stss", 0, 0, u32(syncSamples.length), ...syncSamples.map(u32))]),
              fullBox("stsc", 0, 0, u32(1), u32(1), u32(units.length), u32(1)),
              fullBox("stsz", 0, 0, u32(0), u32(units.length), ...sizes.map(u32)),
              fullBox("stco", 0, 0, u32(1), u32(dataStart)),
            ),
          ),
        ),
      ),
    );

  // Offsets are fixed-size fields, so the sizes do not depend on them.
  const dataStart = ftyp.length + meta(0).length + moov(0).length + 8;
  const mdatSize = 8 + sizes.reduce((n, v) => n + v, 0);

  return concat([
    ftyp,
    meta(dataStart),
    moov(dataStart),
    u32(mdatSize),
    fourcc("mdat"),
    ...units.map((s) => s.data),
  ]);
}

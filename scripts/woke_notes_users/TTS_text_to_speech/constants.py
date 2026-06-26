import json

VOICES = """### American English

- `lang_code='a'` in [`misaki[en]`](https://github.com/hexgrad/misaki)
- espeak-ng `en-us` fallback

| Name | Traits | Target Quality | Training Duration | Overall Grade | SHA256 |
| ---- | ------ | -------------- | ----------------- | ------------- | ------ |
| **af\_heart** | 🚺❤️ | | | **A** | `0ab5709b` |
| af_alloy | 🚺 | B | MM minutes | C | `6d877149` |
| af_aoede | 🚺 | B | H hours | C+ | `c03bd1a4` |
| af_bella | 🚺🔥 | **A** | **HH hours** | **A-** | `8cb64e02` |
| af_jessica | 🚺 | C | MM minutes | D | `cdfdccb8` |
| af_kore | 🚺 | B | H hours | C+ | `8bfbc512` |
| af_nicole | 🚺🎧 | B | **HH hours** | B- | `c5561808` |
| af_nova | 🚺 | B | MM minutes | C | `e0233676` |
| af_river | 🚺 | C | MM minutes | D | `e149459b` |
| af_sarah | 🚺 | B | H hours | C+ | `49bd364e` |
| af_sky | 🚺 | B | _M minutes_ 🤏 | C- | `c799548a` |
| am_adam | 🚹 | D | H hours | F+ | `ced7e284` |
| am_echo | 🚹 | C | MM minutes | D | `8bcfdc85` |
| am_eric | 🚹 | C | MM minutes | D | `ada66f0e` |
| am_fenrir | 🚹 | B | H hours | C+ | `98e507ec` |
| am_liam | 🚹 | C | MM minutes | D | `c8255075` |
| am_michael | 🚹 | B | H hours | C+ | `9a443b79` |
| am_onyx | 🚹 | C | MM minutes | D | `e8452be1` |
| am_puck | 🚹 | B | H hours | C+ | `dd1d8973` |
| am_santa | 🚹 | C | _M minutes_ 🤏 | D- | `7f2f7582` |

### British English

- `lang_code='b'` in [`misaki[en]`](https://github.com/hexgrad/misaki)
- espeak-ng `en-gb` fallback

| Name | Traits | Target Quality | Training Duration | Overall Grade | SHA256 |
| ---- | ------ | -------------- | ----------------- | ------------- | ------ |
| bf_alice | 🚺 | C | MM minutes | D | `d292651b` |
| bf_emma | 🚺 | B | **HH hours** | B- | `d0a423de` |
| bf_isabella | 🚺 | B | MM minutes | C | `cdd4c370` |
| bf_lily | 🚺 | C | MM minutes | D | `6e09c2e4` |
| bm_daniel | 🚹 | C | MM minutes | D | `fc3fce4e` |
| bm_fable | 🚹 | B | MM minutes | C | `d44935f3` |
| bm_george | 🚹 | B | MM minutes | C | `f1bc8122` |
| bm_lewis | 🚹 | C | H hours | D+ | `b5204750` |

### Japanese

- `lang_code='j'` in [`misaki[ja]`](https://github.com/hexgrad/misaki)
- Total Japanese training data: H hours

| Name | Traits | Target Quality | Training Duration | Overall Grade | SHA256 | CC BY |
| ---- | ------ | -------------- | ----------------- | ------------- | ------ | ----- |
| jf_alpha | 🚺 | B | H hours | C+ | `1bf4c9dc` | |
| jf_gongitsune | 🚺 | B | MM minutes | C | `1b171917` | [gongitsune](https://github.com/koniwa/koniwa/blob/master/source/tnc/tnc__gongitsune.txt) |
| jf_nezumi | 🚺 | B | _M minutes_ 🤏 | C- | `d83f007a` | [nezuminoyomeiri](https://github.com/koniwa/koniwa/blob/master/source/tnc/tnc__nezuminoyomeiri.txt) |
| jf_tebukuro | 🚺 | B | MM minutes | C | `0d691790` | [tebukurowokaini](https://github.com/koniwa/koniwa/blob/master/source/tnc/tnc__tebukurowokaini.txt) |
| jm_kumo | 🚹 | B | _M minutes_ 🤏 | C- | `98340afd` | [kumonoito](https://github.com/koniwa/koniwa/blob/master/source/tnc/tnc__kumonoito.txt) |

### Mandarin Chinese

- `lang_code='z'` in [`misaki[zh]`](https://github.com/hexgrad/misaki)
- Total Mandarin Chinese training data: H hours

| Name | Traits | Target Quality | Training Duration | Overall Grade | SHA256 |
| ---- | ------ | -------------- | ----------------- | ------------- | ------ |
| zf_xiaobei | 🚺 | C | MM minutes | D | `9b76be63` |
| zf_xiaoni | 🚺 | C | MM minutes | D | `95b49f16` |
| zf_xiaoxiao | 🚺 | C | MM minutes | D | `cfaf6f2d` |
| zf_xiaoyi | 🚺 | C | MM minutes | D | `b5235dba` |
| zm_yunjian | 🚹 | C | MM minutes | D | `76cbf8ba` |
| zm_yunxi | 🚹 | C | MM minutes | D | `dbe6e1ce` |
| zm_yunxia | 🚹 | C | MM minutes | D | `bb2b03b0` |
| zm_yunyang | 🚹 | C | MM minutes | D | `5238ac22` |

### Spanish

- `lang_code='e'` in [`misaki[en]`](https://github.com/hexgrad/misaki)
- espeak-ng `es`

| Name | Traits | SHA256 |
| ---- | ------ | ------ |
| ef_dora | 🚺 | `d9d69b0f` |
| em_alex | 🚹 | `5eac53f7` |
| em_santa | 🚹 | `aa8620cb` |

### French

- `lang_code='f'` in [`misaki[en]`](https://github.com/hexgrad/misaki)
- espeak-ng `fr-fr`
- Total French training data: <11 hours

| Name | Traits | Target Quality | Training Duration | Overall Grade | SHA256 | CC BY |
| ---- | ------ | -------------- | ----------------- | ------------- | ------ | ----- |
| ff_siwis | 🚺 | B | <11 hours | B- | `8073bf2d` | [SIWIS](https://datashare.ed.ac.uk/handle/10283/2353) |

### Hindi

- `lang_code='h'` in [`misaki[en]`](https://github.com/hexgrad/misaki)
- espeak-ng `hi`
- Total Hindi training data: H hours

| Name | Traits | Target Quality | Training Duration | Overall Grade | SHA256 |
| ---- | ------ | -------------- | ----------------- | ------------- | ------ |
| hf_alpha | 🚺 | B | MM minutes | C | `06906fe0` |
| hf_beta | 🚺 | B | MM minutes | C | `63c0a1a6` |
| hm_omega | 🚹 | B | MM minutes | C | `b55f02a8` |
| hm_psi | 🚹 | B | MM minutes | C | `2f0f055c` |

### Italian

- `lang_code='i'` in [`misaki[en]`](https://github.com/hexgrad/misaki)
- espeak-ng `it`
- Total Italian training data: H hours

| Name | Traits | Target Quality | Training Duration | Overall Grade | SHA256 |
| ---- | ------ | -------------- | ----------------- | ------------- | ------ |
| if_sara | 🚺 | B | MM minutes | C | `6c0b253b` |
| im_nicola | 🚹 | B | MM minutes | C | `234ed066` |

### Brazilian Portuguese

- `lang_code='p'` in [`misaki[en]`](https://github.com/hexgrad/misaki)
- espeak-ng `pt-br`

| Name | Traits | SHA256 |
| ---- | ------ | ------ |
| pf_dora | 🚺 | `07e4ff98` |
| pm_alex | 🚹 | `cf0ba8c5` |
| pm_santa | 🚹 | `d4210316` |
"""

EXAMPLE_TEXT = '''[Kokoro](/kˈOkəɹO/) is an open-weight TTS model with 82 million parameters. Despite its lightweight architecture, it delivers comparable quality to larger models while being significantly faster and more cost-efficient. With Apache-licensed weights, [Kokoro](/kˈOkəɹO/) can be deployed anywhere from production environments to personal projects.'''


# FIXME: json.dumps - replace with yaml?
DEFAULT_FRONTMATTER = {
    "voice": "",
    "text": EXAMPLE_TEXT,
    "output_dir": "(fill in)"
}


DEFAULT_CONTENTS = f"""---
{json.dumps(DEFAULT_FRONTMATTER, indent=4)}
---
- [ ] Generate

# Voices

- from https://huggingface.co/hexgrad/Kokoro-82M/blob/main/VOICES.md on 2026-06-01

{VOICES}
"""

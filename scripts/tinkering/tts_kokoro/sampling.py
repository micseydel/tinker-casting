from time import time, ctime
from kokoro import KPipeline
import soundfile as sf
import torch
pipeline = KPipeline(lang_code='a')
text = '''
[Kokoro](/kˈOkəɹO/) is an open-weight TTS model with 82 million parameters. Despite its lightweight architecture, it delivers comparable quality to larger models while being significantly faster and more cost-efficient. With Apache-licensed weights, [Kokoro](/kˈOkəɹO/) can be deployed anywhere from production environments to personal projects.
'''

#https://huggingface.co/hexgrad/Kokoro-82M/blob/main/VOICES.md
voices = [
    "af_heart", #    🚺❤️             A   0ab5709b
    "af_alloy", #    🚺   B   MM minutes  C   6d877149
    "af_aoede", #    🚺   B   H hours     C+  c03bd1a4
    "af_bella", #    🚺🔥  A   HH hours    A-  8cb64e02
    "af_jessica", #  🚺   C   MM minutes  D   cdfdccb8
    "af_kore", #     🚺   B   H hours     C+  8bfbc512
    "af_nicole", #   🚺🎧  B   HH hours    B-  c5561808
    "af_nova", #     🚺   B   MM minutes  C   e0233676
    "af_river", #    🚺   C   MM minutes  D   e149459b
    "af_sarah", #    🚺   B   H hours     C+  49bd364e
    "af_sky", #  🚺   B   M minutes 🤏     C-  c799548a
    "am_adam", #     🚹   D   H hours     F+  ced7e284
    "am_echo", #     🚹   C   MM minutes  D   8bcfdc85
    "am_eric", #     🚹   C   MM minutes  D   ada66f0e
    "am_fenrir", #   🚹   B   H hours     C+  98e507ec
    "am_liam", #     🚹   C   MM minutes  D   c8255075
    "am_michael", #  🚹   B   H hours     C+  9a443b79
    "am_onyx", #     🚹   C   MM minutes  D   e8452be1
    "am_puck", #     🚹   B   H hours     C+  dd1d8973
    "am_santa", #    🚹   C   M minutes 🤏     D-  7f2f7582
    # british
    "bf_alice", #    🚺   C   MM minutes  D   d292651b
    "bf_emma", #     🚺   B   HH hours    B-  d0a423de
    "bf_isabella", #     🚺   B   MM minutes  C   cdd4c370
    "bf_lily", #     🚺   C   MM minutes  D   6e09c2e4
    "bm_daniel", #   🚹   C   MM minutes  D   fc3fce4e
    "bm_fable", #    🚹   B   MM minutes  C   d44935f3
    "bm_george", #   🚹   B   MM minutes  C   f1bc8122
    "bm_lewis", #    🚹   C   H hours     D+  b5204750

    #spanish
    # "ef_dora",  #    🚺   d9d69b0f
    # "em_alex",  #    🚹   5eac53f7
    # "em_santa",  #   🚹   aa8620cb
    # #french
    # "ff_siwis",  #   🚺   B   <11 hours   B-  8073bf2d
]

for voice in voices:
    print(f"[{ctime()}] Doing voice {voice}... ", end="")
    start = time()
    generator = pipeline(text, voice=voice)
    for i, (gs, ps, audio) in enumerate(generator):
        print(i, gs, ps)
        sf.write(f'{voice}_{i}.wav', audio, 24000)
    took = time() - start
    print(f"took {took}s")

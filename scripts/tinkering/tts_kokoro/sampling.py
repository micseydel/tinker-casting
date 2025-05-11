from time import time, ctime
from kokoro import KPipeline
import soundfile as sf
import torch
pipeline = KPipeline(lang_code='a')
text = '''
[Kokoro](/kˈOkəɹO/) is an open-weight TTS model with 82 million parameters. Despite its lightweight architecture, it delivers comparable quality to larger models while being significantly faster and more cost-efficient. With Apache-licensed weights, [Kokoro](/kˈOkəɹO/) can be deployed anywhere from production environments to personal projects.
'''

#https://doc.voxta.ai/docs/kokoro-tts/
voices = [
    # female
    "af_bella",
    "af_nicole",
    "af_sarah",
    "af_sky",
    "bf_emma",
    "bf_isabella",
    # male
    "am_adam",
    "am_michael",
    "bm_george",
    "bm_lewis",
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
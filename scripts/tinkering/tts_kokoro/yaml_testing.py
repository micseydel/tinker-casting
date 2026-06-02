from ruamel.yaml import YAML

# https://yaml.dev/doc/ruamel.yaml/basicuse/#top

doc = """voice: af_heart
text: "[Kokoro](/kˈOkəɹO/) is an open-weight TTS model with 82 million parameters. Despite its lightweight architecture, it delivers comparable quality to larger models while being significantly faster and more cost-efficient. With Apache-licensed weights, [Kokoro](/kˈOkəɹO/) can be deployed anywhere from production environments to personal projects."
output_dir: /Users/micseydel/obsidian_vaults/deliberate_knowledge_accretion/deliberate_knowledge_accretion_attachments/Kokoro PythonActor Testing/"""

yaml=YAML(typ='safe')   # default, if not specfied, is 'rt' (round-trip)
loaded = yaml.load(doc)
print(loaded)

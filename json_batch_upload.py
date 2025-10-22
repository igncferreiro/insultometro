import json
from openai import OpenAI
import pandas as pd

key =yourapikey
client = OpenAI(
    organization= org-id,
    api_key=key
    )
file_name = 'C:/Datasets/especiales/insultometro/batch_rosada.jsonl'

batch_file = client.files.create(
  file=open(file_name, "rb"),
  purpose="batch"
)


batch_job = client.batches.create(
  input_file_id=batch_file.id,
  endpoint="/v1/chat/completions",
  completion_window="24h"
)
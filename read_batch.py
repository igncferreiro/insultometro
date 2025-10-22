import json
from openai import OpenAI
import pandas as pd

key = yourapikey
client = OpenAI(
    organization=yourorgid,
    api_key=key
    )

output_file_id = "file-Qe2xBzWSuTd53UDjmvMoCD"

content = client.files.content(output_file_id).text

# Extract the custom id and responses
response_results = []
for line in content.split('\n')[:-1]:
        data =json.loads(line)
        custom_id = data.get('custom_id')
        response = data['response']['body']['choices'][0]['message']['content']
        response_results.append([custom_id, response])


response_results = pd.DataFrame(response_results, columns=['custom_id', 'response'])
response_results.to_csv("responses_batch_rosada.csv")
print(response_results)

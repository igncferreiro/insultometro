import json
from openai import OpenAI
import pandas as pd

key = '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'

client = OpenAI(
    organization='org-WqTJdU0yN4FswRiFtWIKBNNq',
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
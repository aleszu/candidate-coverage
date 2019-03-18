import newspaper as newspaper
from newspaper import Article
import pandas as pd
import uuid

data = pd.read_csv('./candidate_coverage.csv', encoding='utf8')

df_done = data[data['text'].str.len() > 0]
df_not_done = data[data['text'].isnull()]

for index,row in df_not_done.iterrows():
    x = row['link']
    article = Article(x, language='en')
    article.download()
    article.parse()
    file_name = str(row['candidate']) + "-" + str(row['date'] + "-" + str(row['outlet'])) + "-" + str(uuid.uuid4())[:5]
    df_not_done.loc[index,'file'] = file_name
    df_not_done.loc[index, 'text'] = article.text
    with open(file_name, 'w') as f:
        f.write(article.text)

complete_data = pd.concat([df_done,df_not_done])

complete_data.to_csv('./candidate_coverage.csv', encoding='utf8', index=False)






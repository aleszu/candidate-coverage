import newspaper as newspaper
from newspaper import Article
import pandas as pd
import uuid

data = pd.read_csv('./candidate_coverage.csv', encoding='utf8')

for index,row in data.iterrows():
    x = row['link']
    article = Article(x, language='en')
    article.download()
    article.parse()
    file_name = str(row['candidate']) + "-" + str(row['date'] + "-" + str(row['outlet'])) + "-" + str(uuid.uuid4())[:5]
    data.loc[index,'file'] = file_name
    data.loc[index, 'text'] = article.text
    with open(file_name, 'w') as f:
        f.write(article.text)

data.to_csv('./candidate_coverage.csv', encoding='utf8', index=False)






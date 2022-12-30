#!/usr/bin/env python
# coding: utf-8

# In[ ]:


# Install a pip package in the current Jupyter kernel - Uncomment if needed
# import sys
# !{sys.executable} -m pip install urllib
# !{sys.executable} -m pip install requests
# !{sys.executable} -m pip install bs4
# !{sys.executable} -m pip install pandas
# !{sys.executable} -m pip install selenium
# !{sys.executable} -m pip install time


# In[2]:


import requests
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from urllib.parse import urljoin
import time


# In[3]:


def get_url1(name):
    
    """
    Gets all urls that are associated with a name of a municipality. As of now not needed.
    """
    
    e.send_keys(name)
    e.send_keys(Keys.ENTER)
    soup = BeautifulSoup(d.page_source)
    d.find_element_by_id('suchfeld').clear()
    time.sleep(0.7)
    links = soup.find('tbody').find_all('a') 
    return [link.get('href') for link in links if 'Votemanager' not in link]


# In[7]:


def get_url2(url1, date):
    
    """ To get the urls specific to the local elections.
    It takes as input the overview url for each municipality from wahlen.regioit
    and returns a list of all Ratswahl-url from its html text.
    """
    
    base = url1
    r = requests.get(url1)
    soup = BeautifulSoup(r.text)
    
    kom_row  = [ r for r in soup.find_all('tr') if 'Kommunalwahlen' in r.text]
    
    res = [link.get('href') for link in BeautifulSoup(str(kom_row)).find_all('a') if date in link]
    
    if res:
        href=res.pop()
        return urljoin(base,href)
    else:
        return np.nan


# In[5]:


def get_url3(url2):
    r = requests.get(url2) 
    soup = BeautifulSoup(r.text)
    base = url2
    hrefs = [link.get('href') for link in soup.find_all('a')]
    
    matches = ['Stadtverordnetenwahl_Hessen', 'Gemeindewahl_Hessen']
    
    href_2=[href for href in hrefs if any(x in href for x in matches)]
    
    if href_2:
        s = href_2.pop()
        return urljoin(base,s)
    else:
        return np.nan


# In[ ]:


def scrape_webpage(data, col_city, col_link, year):
    
    """ Idea: Scrape html tables from the website for names and party affiliation.
        Input: 
            Data = Dataframe to be used
            col_city = Column where the name of the cities are stored
            col_link = Column where the links are stored
            year = Year of the relevant election
            
        Output:
            output_data = Dataframe with scraped information
    
    Comments later: Ignore votes of candidates from previous crawler """
    
    list_cities = data[col_city].unique()
    iterations = len(data[col_link])
    count = 0
    data_url_01 = data[~data[col_link].isna()].reset_index(drop=True)
    data_votes = pd.DataFrame()
    data_votes_2 = pd.DataFrame()
    output_data = pd.DataFrame()
    
    for n in range(len(data_url_01[col_link])):

        print(f'City {n}/{iterations}')

        r = requests.get(data_url_01[col_link][n])

        soup = BeautifulSoup(r.text)
        
        if soup.find_all(lambda tag: tag.name=='table') == []:
            print(f'{list_cities[n]} has no entry')
    
        else:
            table = soup.find_all(lambda tag: tag.name=='table')[-1]
            rows_votes = table.find_all(lambda tag: tag.name=='abbr')
            rows_votes = [row.get_text() for row in rows_votes]

            if rows_votes == []:
                print(f'{list_cities[n]} has no entry')

            else:
                data_votes = pd.DataFrame(data=[rows_votes]).T.rename({0:'candidate'}, axis=1)
                #data_votes['votes'] = data_votes['candidate'].astype(str).str.replace('.','').str.extract(r'([0-9]+[^%])')

                data_votes['candidate'] = data_votes['candidate'].astype(str).str.replace('ß', 'ss').str.replace('Ü', 'U').str.replace('ü', 'ue').str.replace('ä', 'ae').str.replace('ö', 'oe').str.replace('.', '').str.replace('é', 'e')
                #data_votes['candidates'] = data_votes['candidate'].str.extract(r'([A-Z][a-z]+[\s][A-Z][a-z]+[-]?[A-Z]*[a-z]*[,]*[\s]*[A-Z]*[a-z]*)')

                #data_votes_2['votes'] = data_votes[data_votes["votes"].str.contains('<', na=False)].reset_index(drop=True)['votes'].str.replace('<', '')
                #data_votes_2['candidates'] = data_votes['candidates'].dropna().reset_index(drop=True)
                data_votes['city'] = list_cities[n]
                data_votes['year'] = year
                output_data = output_data.append(data_votes).dropna().reset_index(drop=True)
    
    return output_data


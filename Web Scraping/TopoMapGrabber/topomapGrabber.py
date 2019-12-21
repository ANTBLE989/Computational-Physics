

import requests
import os, bs4

os.mkdir('topomap')
txt = open('G:/TopoMaps/TopoMapList.txt')
urlist = []
errlist = []
for i in txt:
    a = i.split('\n')
    urlist.append(a[0])

    
site = 'http://www.namria.gov.ph/'
for url in urlist:
    website = site + url
    # download the website locally
    print('Downloading the page %s' % website)
    res = requests.get(website)
    
    if res.status_code == 404:
        errlist.append(url)
        continue
    else:
        res.raise_for_status()
        soup = bs4.BeautifulSoup(res.text)
        # Find the URL of the page image
        img = soup.select('img')
        if img == []:
            print('Could not find any image.')
        else:
            try:
                imgUrl = site + img[0].get('src')
                # download the image
                print('Downloading the image %s ..' % (imgUrl))
                res = requests.get(imgUrl)
                if res.status_code == 404:
                    errlist.append(url)
                    continue 
                else:
                    res.raise_for_status()
                    # Save the image to the TopoMap Folder
                    imageFile = open(os.path.join('topomap',os.path.basename(imgUrl)),'wb')
                    for chunk in res.iter_content(100000):
                        imageFile.write(chunk)
                    imageFile.close()
                    
            except requests.exceptions.MissingSchema:
                print('Something went wrong on the page')
                continue
        
    
   
    print website
    print('Done.')
    
    

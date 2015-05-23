""" 
ICA 2015 RETRIEVAL SCRIPT

Gets all paper / presentations for the ICA 2015 conference in Puerto Rico

Please note: requires the lxml library, which is not available through pip,
                   but is available through aptitude on linux.
				   "aptitude install -y python-lxml"

"""


import requests
import json
from lxml.html import fromstring

s = requests.Session()

home_url = "http://convention2.allacademic.com/one/ica/ica15/"

home_dom = fromstring(s.get(home_url).text)

index_url = home_dom.xpath("//li/a")[6].get("href")

index_page = s.get(index_url)

index_dom  = fromstring(index_page.text)

divisions = index_dom.xpath("//ul/li/a")

papers = []

for division in divisions:
	if not division.text: continue
	division_name = division.text
	
	print
	print "="*5 + " DIVISION: %s" %division_name+ "="*5
	
	div_page = s.get(division.get("href"))
	div_dom  = fromstring(div_page.text)
	
	session_links = [link[2] for link in div_dom.iterlinks() if "selected_session"  in link[2]]
	
	for session in session_links:
		session_page = s.get(session)
		session_dom  = fromstring(session_page.text)
		
		session_name    = session_dom.xpath("//h3/text()")[0]
		session_type      = session_dom.xpath("//div/div/p/text()")[0]
		try:       session_abstact  = session_dom.xpath("//div/div/blockquote/p/text()")[0]
		except: session_abstract = ""
		session_unit       = session_dom.xpath("//li['ui-first-child ui-last-child']/a/p/strong/text()")[0]
		session_time      = session_dom.xpath("//div/strong/text()")[0]
		
		print
		print "-"*6 + " Session: %s" %session_name  + "-"*6
		
		paper_links        = [link[2] for link in session_dom.iterlinks() if 'selected_paper' in link[2]]
		for paper in paper_links:
			paper_page = s.get(paper)
			paper_dom  = fromstring(paper_page.text)
			if "multiple events for single paper violation" in paper_dom.text_content(): continue
			
			paper_title = paper_dom.xpath("//h3/text()")[0]
			try: paper_abstract = paper_dom.xpath("//blockquote/p/text()")[0]
			except: paper_abstract = ""
			
			
			print "\t%s" %paper_title 
			
			author_elements = paper_dom.xpath("//li/a")[14:]
			authors = []
			for a in author_elements:
				try:       firstname, initials, name = a.xpath('p/i/text()')
				except: 
					try:
						firstname, name = a.xpath('p/i/text()')
						initials = ""
					except:
						continue
				university  = a.xpath('p/text()')[-1]
				authors.append(dict(
					firstname = firstname,
					initials     = initials,
					name      = name,
					university= university
				))
			
			paper = dict(
				division               = division_name,
				session_name     = session_name,
				session_abstract  = session_abstract,
				session_unit        = session_unit,
				session_type       = session_type,
				paper_title          = paper_title,
				paper_abstract    = paper_abstract,
				paper_authors     = authors,
				from_url              = paper
			)
			print paper['paper_title']
			for a in paper['paper_authors']: print a
			papers.append(paper)

json.dump(papers, open("ica_papers_2015.json","w"))
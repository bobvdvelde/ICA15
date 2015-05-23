"""

ICA 2015 to network script

Takes the scraped JSON file and converts it to three network files

"""

import csv
import json
import codecs

dr = csv.DictReader(open("university_recode_map.tsv"), delimiter="\t")
university_recode_map = {u["name"]:u["Change"] for u in dr }

def clean_university(university):
	"remove leading comma and extra spaces"
	university = university.replace(",",'').replace("University","U").strip()
	if university_recode_map.get(university,""):
		university = university_recode_map[university]
	return university
	
def auth_to_string(author):
	return "%s, %s" %(author['name'], author['firstname'])
	
def increment(d,v):
	if not v in d:
		d[v] = 1
	else:
		d[v] +=1
		
def dict_to_string(d):
	if type(d) !=  dict: return d
	outstring = ""
	for k,v in d.iteritems():
		outstring += "%s : %s \n" %(k,v)
	return outstring

def cs(text):
	clean_string = []
	for char in text:
		try: clean_string.append(str(char))
		except: pass
	return ''.join(clean_string)

papers = json.load(open("ica_papers_2015.json"))
		
authornet  	= dict(edgelist=dict(), nodelist = dict())
divisionnet 	= dict(edgelist=dict(), nodelist = dict())
universitynet	= dict(edgelist=dict(), nodelist = dict())

# pre-process content

for p in papers:
	for a in p["paper_authors"]:
		
		# Building authornet 
		author_string = auth_to_string(a)
		
		# Building co-author edges
		if not author_string in authornet:
			authornet["edgelist"][author_string] = dict()
		for alt in p["paper_authors"]:
			increment(authornet["edgelist"][author_string],auth_to_string(alt))
		# Building author properties
		if not author_string in authornet["nodelist"]:
			authornet["nodelist"][author_string] = dict(papers="",
			divisions=dict(), 
			paper_no=0,
			university=clean_university(a["university"]))
		# 1. paper titles
		authornet["nodelist"][author_string]["papers"] += "%s\n" %p['paper_title']
		# 2. divisions participated in
		increment(authornet["nodelist"][author_string] ["divisions"], p["division"])
		authornet["nodelist"][author_string]["paper_no"] +=1
		# build universitynet
		univ = clean_university(a["university"])
		for alt_univ in [clean_university(a['university']) for a in p['paper_authors']]:
			if univ not in universitynet["edgelist"]: 
				universitynet["edgelist"][univ]=dict()
			increment(universitynet['edgelist'][univ],alt_univ)
		if not universitynet["nodelist"].get(univ,""):
			universitynet["nodelist"][univ]=dict(divisions=dict(),authors=dict(), panels=dict(),session_types=dict())
		
		increment(universitynet["nodelist"][univ]["authors" ], author_string)
		
		
		# division universities
		if not p["division"] in divisionnet["nodelist"].keys():
			divisionnet["nodelist"][p["division"]] = dict(
			authors = dict(),
			panels = dict(),
			universities = dict(),
			session_types=dict()
			)
		increment(divisionnet["nodelist"][p["division"]]["authors"], author_string)
		increment(divisionnet["nodelist"][p["division"]]["universities"], univ)
		
		
	# do paper level increments (i.e. once per paper instead of once per author)
	increment(universitynet["nodelist"][univ]["divisions" ], p["division"])
	increment(universitynet["nodelist"][univ]["panels" ], p["session_name"])
	increment(universitynet["nodelist"][univ]["session_types" ], p["session_type"])
	
	increment(divisionnet["nodelist"][p["division"]]["session_types"], p["session_type"])
	increment(divisionnet["nodelist"][p["division"]]["panels"], p["session_name"])


# output networks
print "Outputting author network"
## Authors
fieldnames = [ "id", "label","papers","paper_no","divisions","university"]
dr = csv.DictWriter(open("author_nodelist.csv","w"), fieldnames)
dr.writeheader()
for k,v in authornet["nodelist"].iteritems():
	dr.writerow(dict(
		id = cs(k),
		label = cs(k),
		university = cs(v["university"]),
		paper_no = v["paper_no"],
		papers = cs(v["papers"]),
		divisions = cs(dict_to_string(v["divisions"]))
		))


fieldnames = ["source","target","weight"]
dr = csv.DictWriter(codecs.open("author_edgelist.csv","w",encoding="utf-8"), fieldnames)
dr.writeheader()
for k,v in authornet["edgelist"].iteritems():
	for target, weight in v.iteritems():
		dr.writerow(dict(
		source = cs(k),
		target = cs(target),
		weight = weight
		))

## Divisions
print "Outputting division network"
fieldnames = ["Id","Label","Authors","Universities","Panels","Types"]
dr = csv.DictWriter(codecs.open("division_nodelist.csv","w",encoding="utf-8"), fieldnames)
dr.writeheader()
for k,v in divisionnet["nodelist"].iteritems():
	dr.writerow(dict(
		Id					= k,
		Label 			= k,
		Authors 		= len(v["authors"]),
		Universities 	= len(v["universities"]),
		Panels   		= len(v["panels"]),
		Types  			= cs(dict_to_string(v["session_types"]))
		))

fieldnames = ["source","target","weight"]
dr = csv.DictWriter(codecs.open("division_edgelist.csv","w",encoding="utf-8"), fieldnames)
dr.writeheader()
for k,v in divisionnet["nodelist"].iteritems():
	for ki,vi in divisionnet["nodelist"].iteritems():
		dr.writerow(dict(
			source = cs(k),
			target =  cs(ki),
			weight = len(set.intersection(set(v["authors"]),set(vi["authors"])))
			))

## Universities
print "Outputting university network"
fieldnames = ["source","target","weight"]
dr = csv.DictWriter(codecs.open("university_edgelist.csv","w",encoding="utf-8"), fieldnames)
dr.writeheader()
for k,v in universitynet["edgelist"].iteritems():
	for ki, vi in v.iteritems():
		if not k or not ki: continue
		dr.writerow(dict(
			source 	= cs(k),
			target 	= cs(ki),
			weight 	= vi,
		))
		
fieldnames = [ "Id" , "Label", "Authors","Divisions","Panels","Session_types"]
dr = csv.DictWriter(codecs.open("university_nodelist.csv","w",encoding="utf-8"), fieldnames)
dr.writeheader()
for k,v in universitynet["nodelist"].iteritems():
	if not k: continue
	dr.writerow(dict(
	Id					= cs(k),
	Label			= cs(k),
	Authors 		= len(v["authors"]),
	Divisions 		= cs(dict_to_string(v["divisions"])),
	Panels 			= cs(dict_to_string(v["panels"])),
	Session_types= cs(dict_to_string(v["session_types"])) 
	))
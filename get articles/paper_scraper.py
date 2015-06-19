"""


This script mines all the articles from the ICA 2015 

ALL PAPERS ARE BELONGS TO US*

* for academic purposes only


"""

import requests, json, os, shutil
from lxml.html import fromstring
from requests.auth import HTTPBasicAuth

missing_file = "Missing_papers.txt"

# make sure the pdf folder exists & missings is empty
if not "pdfs" in os.listdir("."): os.mkdir("pdfs")
remove_punctuation = lambda x: ''.join([char for char in x if char not in ",.'\"[]{}!?:;~`@#$%^&*()+_=<>|\\"])
def ascii_proof(x):
	proof = list()
	for char in x:
		try: proof.append(str(char))
		except: pass
	return "".join(proof)
	
if missing_file in os.listdir("."): os.remove(missing_file)

base_url  = "http://convention2.allacademic.com/one/ica/ica15/index.php?cmd=ica15_access"
password = "SanJuan2015"

s = requests.Session()
s.auth = HTTPBasicAuth('guest_downloader',password)


# LOGIN IN TO PAPER SITE
print "Logging in...",
res = s.get(base_url)

element = fromstring(res.text).xpath("//form[@action]")[0]
element.make_links_absolute(res.url)
login = element.values()[1]
sessid = login.split("=")[-1]

data = dict( 
	password  =password,
	usr_name ="guest_downloader",
	cmd          = "Logon Access",
	submit       = "Login",
	confirm_txt="confirmation_find_user.txt",
	PHPSESSID=sessid
	
)

params = dict(
	
)
print "DONE"
# POST LOGIN LANDING PAGE

login_response = s.post(login,data)
login_page = fromstring(login_response.text)
individual_submissions_page_link = login_page.xpath("//a[@class='mainmenu_text']")[1].values()[1]

# START GOING THROUGH THE ITEMS
i = 0
while individual_submissions_page_link:
	i +=1
	submission_page_response = s.get(individual_submissions_page_link)
	submission_page = fromstring(submission_page_response.text)
	
	links = [ l.values()[1] for l in submission_page.xpath("//li/a[@class='fieldtext']") ]
	for num,link in enumerate(links):
		paper_response = s.get(link)
		paper_page = fromstring(paper_response.text)
		print "getting paper...",
		title = paper_page.xpath("//font[@class='headingtext']/text()")[3]
		saveable_title = ascii_proof(remove_punctuation(title))
		
		
		try: 
			paper_link_holder = paper_page.xpath("//blockquote[@class='tight']/a[@class='fieldtext']")[0]
			paper_link_holder.make_links_absolute(paper_response.url)
			paper_link = paper_link_holder.values()[3]
		except: 
			print "No paper (%i : %i)" %(i,num)
			with open(missing_file,"a+") as f:
				f.writelines(["%i (%i): %s\n" %(i,num,saveable_title)])
			continue
		
		paper_pdf = s.get(paper_link, stream=True)
		with open("pdfs/"+saveable_title+".pdf", 'wb') as f:
			paper_pdf.raw.decode_content = True
			shutil.copyfileobj(paper_pdf.raw, f)
		print saveable_title
		
	bns = bns = submission_page.xpath("//div[@class='iterator']/form/div/a[@class='fieldtext']")
	nextbuttons = [b for b in bns if b.text == "Next"]
	if nextbuttons:
		nextbutton = nextbuttons[0]
		nextbutton.make_links_absolute(submission_page_response.url)
		individual_submissions_page_link = nextbutton.values()[1]
	else:	
		individual_submissions_page_link = False



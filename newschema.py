# -*- coding: utf-8 -*-
"""
Created on Thu Sep  3 23:47:13 2015

"""

import xml.etree.ElementTree as ET
import MySQLdb as mdb
conn=mdb.connect('localhost','root',db='new_bug_report')
cur=conn.cursor()
product_list=['./mozilla/Bugzilla']#,'./mozilla/Core','./mozilla/Firefox','./mozilla/Thunderbird','./eclipse/CDT','./eclipse/JDT', './eclipse/PDE', './eclipse/Platform']
attribute_list=['assigned_to', 'bug_status', 'cc', 'component', 'op_sys', 'priority', 'product', 'resolution', 'severity', 'short_desc', 'version']
obj={}

'''
Now insert into reports table
'''
obj.clear()
for prod in product_list:
    filenm=prod+'/reports.xml'
    tree = ET.parse(filenm)
    root = tree.getroot()
    for x in root:
        obj.clear()
        obj['report_id']=int(x.attrib['id'])
        for child in x:
            obj[child.tag]=child.text
        print obj
        cur.execute("""INSERT INTO Bugzill_reports( report_id, open_time, reporter ) VALUES (%s,%s,%s);""",(str(obj['report_id']),str(obj['opening_time']),str(obj['reporter'])))
            
conn.commit()          

'''
Adding into products table
'''

for prod in product_list:
    for i in attribute_list:
        filenm=prod+'/'+i+'.xml'
        tree = ET.parse(filenm)
        root = tree.getroot()

        for x in root:
            obj.clear()
            obj['report_id']=int(x.attrib['id'])
            for t in x:
                for child in t:
                    if child.tag=='when':
                        obj[child.tag]=int(child.text)
                    else:
                        obj[child.tag]=child.text
                print obj
                try:
                    cur.execute("""INSERT INTO Bugzilla_updates( report_id, timestamp,attribute,  what ) VALUES (%s,%s,%s,%s);""",((obj['report_id']),(obj['when']),i,(obj['what'])))
                except(UnicodeEncodeError,RuntimeError):
                    cur.execute("""INSERT INTO Bugzilla_updates( report_id, timestamp, attribute, what ) VALUES (%s,%s,%s,NULL);""",((obj['report_id']),(obj['when']), i))
                    
        conn.commit()  


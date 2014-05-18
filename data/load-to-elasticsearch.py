#!/usr/bin/python

import json
import urllib2
from pprint import pprint

# sends a python dictionary to elasticsearch instance
def load_to_elasticsearch(docId, doc):
  opener = urllib2.build_opener(urllib2.HTTPHandler)
  request = urllib2.Request('http://localhost:9200/doctors/doctor/' + str(docId), data=json.dumps(doc))
  request.add_header('Content-Type', 'text/json')
  request.get_method = lambda: 'PUT'
  url = opener.open(request)


# load data into json objects
json_data=open('raw/doctors.json')
data = json.load(json_data)
json_data.close()

# transform objects
for feature in data['features']:
  doc = {
    'objectId' : feature['properties']['OBJECTID'],
    'discipline' : feature['properties']['FACH'],
    'title' : feature['properties']['ANREDE'],
    'honoraryTitle' : feature['properties']['VERLTITEL'],
    'degree' : feature['properties']['GRDTITEL'],
    'firstName' : feature['properties']['VORNAME'],
    'lastName' : feature['properties']['NACHNAME'],
    'address' : {
      'street' : feature['properties']['STRASSE'],
      'zip' : feature['properties']['PLZ'],
      'city' : feature['properties']['ORT'],
      'location' : feature['geometry']['coordinates'],
      'phone' : feature['properties']['TELEFON'],
      'fax' : feature['properties']['FAX'],
      'email' : feature['properties']['EMAIL'],
    },
    'insurance' : []
  }

  # build the array of insurances
  if feature['properties']['SVA'] is not None:
    doc['insurance'].append('SVA');

  if feature['properties']['BVA'] is not None:
    doc['insurance'].append('BVA');

  if feature['properties']['GKK'] is not None:
    doc['insurance'].append('GKK');

  if feature['properties']['KFAG'] is not None:
    doc['insurance'].append('KFAG');

  if feature['properties']['KFAW'] is not None:
    doc['insurance'].append('KFAW');

  if feature['properties']['SVB'] is not None:
    doc['insurance'].append('SVB');

  if feature['properties']['OEDA'] is not None:
    doc['insurance'].append('OEDA');

  if feature['properties']['VA'] is not None:
    doc['insurance'].append('VA');

  pprint(doc)
  load_to_elasticsearch(feature['properties']['OBJECTID'], doc)

from __future__ import division, print_function, with_statement
import codecs
import hashlib
import itertools
import logging
import os
import os.path
from stat import S_IRUSR
from urllib2
import urlopen, Request, HTTPError
import os, sys, time, datetime
from multiprocessing.pool import ThreadPool
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from variables import *
from simplejson import JSONDecodeError
from flask import Flask, url_for, session, request, redirect, Response
if __name__ == '__main__':
 print 'Hello!'
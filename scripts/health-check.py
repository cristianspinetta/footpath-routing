#!/usr/bin/env python
import urllib2
import time
from threading import Thread
from subprocess import call
import sys
import os

class HealthCheckStarter(Thread):
  def __init__(self, URL, timeout, pidFile):
    Thread.__init__(self)
    self.timeout = timeout
    self.URL = URL
    self.pidFile = pidFile
    self.result = ''
    self.code = 0
    self.error = ''

  def run(self):
    fp = ''
    t0 = time.time()
    pidFile=open(self.pidFile,'r')
    pid=pidFile.read()
    while 1:
      try:
        fp = urllib2.urlopen(urllib2.Request(self.URL), timeout = 1800)
        self.result = fp.read()
        self.code = fp.getcode()
        break
      except urllib2.HTTPError, e:
        if e.code == 503:
          time.sleep(1)
          remaining_time = int(self.timeout - (time.time() - t0))
          if remaining_time % 5 == 0: info(str(remaining_time) + ' seconds remaining.')
        else:
          self.error = 'Response Status: ' + e.code
          break
      except:
        if not pid_is_running(pid):
          self.error = 'Application has failed to startup'
          break
        if "Connection refused" in str(sys.exc_info()[1]):
          time.sleep(1)
          remaining_time = int(self.timeout - (time.time() - t0))
          if remaining_time % 5 == 0: info(str(remaining_time) + ' seconds remaining.')
        else:
          self.error = str(sys.exc_info()[1])
          break
      finally:
        if fp: fp.close()
      if time.time() - t0 > 1800:
        self.error = 'Timeout'
        break

def pid_is_running(pid):
    try:
        os.kill(int(pid), 0)
    except OSError:
        return False
    else:
        return True

def info(text):
  print '[INFO]  ' + text
  sys.stdout.flush()

def error(text):
  print '[ERROR] ' + text
  sys.stdout.flush()

def healthCheck(url, timeout, pidFile):
  thread = HealthCheckStarter(url, timeout, pidFile)
  thread.start()

  thread.join(timeout)
  if thread.isAlive():
    thread._Thread__stop()
    error('Health check timed out!')
    return 1
  else:
    if (not thread.error):
      if(thread.code == 200):
        info('Health check OK!')
        info('Health check response: ' + thread.result)
        return 0
      else:
        error('Health check FAILED!')
        error('Health check status code: ' + str(thread.code))
        error('Health check response: ' + thread.result)
        return 1
    else:
      error('Health check FAILED!')
      error(thread.error)
      call(["cat","logs/app.log"])
      return 1

def main():
  call(["pwd"])
  exit_code = healthCheck(sys.argv[1], int(sys.argv[2]), sys.argv[3])
  exit(exit_code)

if __name__ == '__main__':
  main()
    
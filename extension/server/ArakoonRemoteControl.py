"""
This file is part of Arakoon, a distributed key-value store. Copyright
(C) 2010 Incubaid BVBA

Licensees holding a valid Incubaid license may use this file in
accordance with Incubaid's Arakoon commercial license agreement. For
more information on how to enter into this agreement, please contact
Incubaid (contact details can be found on www.arakoon.org/licensing).

Alternatively, this file may be redistributed and/or modified under
the terms of the GNU Affero General Public License version 3, as
published by the Free Software Foundation. Under this license, this
file is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.

See the GNU Affero General Public License for more details.
You should have received a copy of the
GNU Affero General Public License along with this program (file "COPYING").
If not, see <http://www.gnu.org/licenses/>.
"""
import socket
import logging
import struct


_COLLAPSE_TLOGS = 0x14
_DOWNLOAD_DB = 0x1b
_MAGIC   = 0xb1ff0000
_VERSION = 0x00000001

def _int_to(i):
    r = struct.pack("I", i)
    return r

def _int_from(buff,pos):
    r = struct.unpack_from("I",buff, pos)
    return r[0], pos + 4

def _int64_from(buff,pos):
    r = struct.unpack_from("Q",buff,pos)
    return r[0], pos + 8

def _string_to(s):
    size = len(s)
    r = struct.pack("I%ds" % size, size, s)
    return r

def _prologue(clusterId, sock):
    m  = _int_to(_MAGIC)
    m += _int_to(_VERSION)
    m += _string_to(clusterId)
    sock.sendall(m)

def _receive_all(sock,n):
    todo = n
    r = ""
    while todo:
        chunk = sock.recv(todo)
        if chunk == "" :
            raise RuntimeError("Not enough data on socket. Aborting...")
        todo -= len(chunk)
        r += chunk
    return r

def _receive_int(sock):
    sizes = _receive_all(sock,4)
    i,_  = _int_from(sizes,0)
    return i

def _receive_int64(sock):
    buf = _receive_all(sock, 8)
    i64,_ = _int64_from(buf,0)
    return i64
    
def _receive_string(sock):
    size = _receive_int(sock)
    s = _receive_all(sock,size)
    return s

def check_error_code(sock):
    rc = _receive_int(sock)
    if rc:
        msg = _receive_string(sock)
        raise Exception (msg)

    
def collapse(ip, port, clusterId, n):
    """
    tell the node listening on (ip, port) to collapse, and keep n tlog files 
    @type ip: string
    @type port: int
    @type n: int > 0
    @type clusterId:string
    @param clusterId: must match cluster id of the node
    """
    if n < 1: 
        raise ValueError("%i is not acceptable" % n)
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sa = (ip, port)
    s.connect(sa)
    
    try:
        _prologue(clusterId, s)
        cmd  = _int_to(_COLLAPSE_TLOGS | _MAGIC)
        cmd += _int_to(n)
        s.send(cmd)
        check_error_code(s)
        collapse_count = _receive_int(s)
        for i in range(collapse_count):
            check_error_code(s)
            took = _receive_int64(s)
            logging.info("took %l", took)
    finally:
        s.close()
        
def downloadDb(ip, port, clusterId, location):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sa = (ip, port)
    s.connect(sa)
    
    def log ( msg ):
        f = open('/tmp/log', 'a')
        f.write(msg + "\n")
        f.close()
    try:
        with open(location,'w+b') as db_file:
            _prologue(clusterId, s)
            cmd  = _int_to(_DOWNLOAD_DB | _MAGIC)
            s.send(cmd)
            check_error_code(s)
            db_size = _receive_int64(s)
            log ("DB size is %dL bytes" % db_size )
            while (db_size > 0 ) :
                chunkSize = min(4*1024, db_size)
                chunk = _receive_all(s, chunkSize)
                log ("Writing %s bytes to file\n" % str(len(chunk)))
                db_size -= len(chunk)
                db_file.write(chunk)
    finally:
        s.close()
    
    
    


## Basic Arakoon commands
### List all Arakoon databases
All Arakoon DBs are running as a service with as name `ovs-arakoon-<databasename>`.

```
ovs monitor services

...
ovs-arakoon-ovs-bknd-poc01-abm start/running, process 16308
ovs-arakoon-ovs-bknd-poc01-nsm_0 start/running, process 20064
ovs-arakoon-ovsdb start/running, process 19901
ovs-arakoon-voldrv start/running, process 20400
...
```
In this case we have 4 Arakoon clusters running: ovsdb, voldrv, ovs-bknd-poc01-nsm_0 and ovs-bknd-poc01-abm.


### Start an Arakoon cluster
To start the ovsdb cluster:

```
start ovs-arakoon-ovsdb
```

### Restart an Arakoon cluster
To restart the ovsdb cluster:

```
restart ovs-arakoon-ovsdb
```


### Stop an Arakoon cluster
To start the ovsdb cluster:
```
stop ovs-arakoon-ovsdb
```

### Check if an Arakoon cluster has a master
An Arakoon cluster always has one master node. Arakoon uses Multi-Paxos to decide which node becomes the master node. In the below code sample we will check if the ovsdb is functioning correctly and has a master node selected.
```
arakoon --who-master -config /opt/OpenvStorage/config/arakoon/ovsdb/ovsdb.cfg
```
The result will be the id of the selected master node.

###  Basic Set/Get/Delete
```
cluster$> arakoon -config /opt/OpenvStorage/config/arakoon/ovsdb/ovsdb.cfg --set some_key some_value
cluster$> arakoon -config /opt/OpenvStorage/config/arakoon/ovsdb/ovsdb.cfg --get some_key
"some_value"

cluster$> arakoon -config /opt/OpenvStorage/config/arakoon/ovsdb/ovsdb.cfg --delete some_key
cluster$> arakoon -config /opt/OpenvStorage/config/arakoon/ovsdb/ovsdb.cfg --get some_key
Fatal error: exception Arakoon_exc.Exception(5, "some_key")
```

### Collapsing TLogs
When the number of updates is higher than the number of additions, the collection of TLogs keeps on growing. You can however reduce the space needed on disk if you collapse old TLogs into a head database.

To collapse the TLogs:
```
arakoon --collapse <tlog_dir> <n>
```
Where the `<tlog_dir>` is defined in the Arakoon configuration file and `<n>` is the number of TLogs that must remain. For example when you have 40 TLog files in /tmp/Node_0 and you want to keep the five most recent TLogs, run this command:
```
arakoon --collapse /tmp/Node_0 5
```
### Investigating TLogs
It is not possible to read TLogs as stored on the file system. If you want human-readable output, you need a dump of the TLog.
```
arakoon --dump-tlog <path_to_tlog_file>
```
For example:

```
arakoon --dump-tlog /opt/OpenvStorage/db/arakoon/ovsdb/tlogs/003.tlog

308476:(Vc ([Delete         ;"ovs-watcher-026829a5-2792-4f53-b9f3-ea730ccef3ac";],false)
308477:(Vm (tGLpPNHEDxNOOqOZ,1449145497.000000))
308478:(Vc ([NOP;],false)
308479:(Vc ([Set            ;"ovs_celery_beat_lock";55;"...";],false)
308480:(Vc ([Set            ;"ovs-watcher-fd1159ae-df01-48c2-92ff-69b4a6df94d0";15;"...";],false)
308481:(Vc ([Delete         ;"ovs-watcher-fd1159ae-df01-48c2-92ff-69b4a6df94d0";],false)
308482:(Vc ([Set            ;"ovs-watcher-c400c259-05f0-4a2a-a665-8b11195ee2df";15;"...";],false)
308483:(Vc ([Delete         ;"ovs-watcher-c400c259-05f0-4a2a-a665-8b11195ee2df";],false)
308484:(Vc ([Set            ;"ovs-watcher-c518c1ab-c606-4b9e-a119-d3a0fda7efc6";15;"...";],false)
308485:(Vc ([Delete         ;"ovs-watcher-c518c1ab-c606-4b9e-a119-d3a0fda7efc6";],false)
308486:(Vc ([Set            ;"ovs_data_storagerouter_5f081738-4447-4dff-85a3-b4f537fab9ce";879;"...";],false)
308487:(Vc ([Set            ;"ovs_data_storagerouter_60525280-7159-4dd6-a3d0-331a0742490a";879;"...";],false)
308488:(Vc ([Set            ;"ovs_data_storagerouter_8cbc1463-28ef-4813-a33d-1972356dd6c0";879;"...";],false)
308489:(Vm (tGLpPNHEDxNOOqOZ,1449145502.000000))
308490:(Vc ([NOP;],false)
308491:(Vc ([Set            ;"ovs_data_storagerouter_5f081738-4447-4dff-85a3-b4f537fab9ce";879;"...";],false)
308492:(Vc ([Set            ;"ovs_data_storagerouter_5f081738-4447-4dff-85a3-b4f537fab9ce";879;"...";],false)

...
```

### Backing up the database
You can perform a hot backup from a live slave node from the command line. If you also backup the tlogs and conifguration file of the node, you have all the required data to be able to rebuild a node. This type of backup allows you to be able to recover from multi-node failure.

The backup of the database can be done by running the following command:
```
# arakoon --backup-db <cluster_id> <ip> <port> <location>
# e.g.
arakoon --backup-db ricky 127.0.0.1 7080 /mnt/drv/2011-19-07/mybackup.db
```

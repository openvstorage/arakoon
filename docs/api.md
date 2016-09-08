## A simple example using the OVS Python Client
How to manage Arakoon through the  python API is best explained through a simple example. The code below creates a client and grows the cluster:

Start the OVS client
```
ovs
```

Extend the cluster with a new node (new_ip)
```
from ovs.extensions.db.arakoon.ArakoonInstaller import ArakoonInstaller
ArakoonInstaller.extend_cluster(master_ip, new_ip, cluster_name, exclude_ports, base_dir)

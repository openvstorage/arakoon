#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <netdb.h>
#include <unistd.h>
#include <errno.h>

#include "arakoon.h"
#include "arakoon_protocol.h"
#include "util.h"

const char no_master_msg[] = "Could not determine master.";

ara_cluster_t
create_empty_cluster(size_t cluster_id_size, const char* cluster_id) {
	ara_cluster_t ret;
	bound_strncpy (ret.cluster_id, cluster_id, MXLEN_CLUSTERNAME+1);
	ret.cluster_id_size = cluster_id_size;
	ret.nodes = NULL;
	ret.last_error[0]='\0';
	ret.master_sock = -1;
	ret.master_name[0] = '\0';
	ret.cmd_buf = checked_malloc(MXLEN_REQ);
	return ret;
}

ara_cluster_t
add_node_to_cluster (ara_cluster_t cluster, const char* node_name, const char* address, int port) {
	ara_node_id_t* new_node = (ara_node_id_t*) checked_malloc (sizeof(ara_node_id_t));
	bound_strncpy( new_node->name, node_name, MXLEN_NODENAME + 1);
	bound_strncpy( new_node->address, address, MXLEN_HOSTNAME + 1);
	new_node->port = port;
	ara_node_list_elem_t*  new_list_elem =
			(ara_node_list_elem_t*) checked_malloc( sizeof(ara_node_list_elem_t));
	new_list_elem->cur = new_node;
	new_list_elem->next = NULL;

	ara_node_list_elem_t* last_node = cluster.nodes;
	if ( ! last_node ) {
		cluster.nodes = new_list_elem;
	} else {
		while( last_node->next ) {
			last_node=last_node->next;
		}
		last_node->next = new_list_elem;
	}
	return cluster;
}

void cleanup_cluster(ara_cluster_t* cluster) {
	ara_node_list_elem_t* iter, *prev;
	iter=cluster->nodes;
	while (iter) {
		prev = iter;
		iter = iter->next;
		free(prev->cur);
		free(prev);
	}
	cluster->nodes = NULL;
        free(cluster->cmd_buf);
	bound_strncpy(cluster->cluster_id,"",MXLEN_CLUSTERNAME);
}

void print_cluster(ara_cluster_t clu) {
	printf ("Cluster details for '%s':\n", clu.cluster_id);
	ara_node_list_elem_t* iter = clu.nodes;
	int i = 0;
	while (iter) {
		printf("%d: node %s at (%s,%d)\n",i ,iter->cur->name, iter->cur->address, iter->cur->port );
		iter=iter->next;
		i++;
	}
}

int send_prologue( ara_cluster_t* cluster, int* sock ) {
	char prologue_buf [MXLEN_PROLOGUE];
	size_t prologue_size;
	int rc = RC_SUCCESS;

	prologue_size = build_prologue(MXLEN_PROLOGUE, prologue_buf, cluster);
	if (prologue_size > MXLEN_PROLOGUE ) {
		rc = RC_BUF_TOO_SMALL;
	}
	if ( rc == RC_SUCCESS ) {
		rc = send_n_bytes( sock, prologue_size, prologue_buf, get_timeout(), MXLEN_ERRMSG, cluster->last_error);
	}
	return rc;
}

int open_connection(ara_cluster_t* cluster, const char* node_name, int*sock ) {
	ara_node_list_elem_t* iter = cluster->nodes;
	while (iter) {
		if( strcmp(iter->cur->name, node_name) == 0) {
			int rc = open_socket( iter->cur->address, iter->cur->port, sock, MXLEN_ERRMSG, cluster->last_error);
			if (rc == RC_SUCCESS ) {
				rc = send_prologue( cluster, sock );
			}
			return rc;
		}
		iter = iter->next;
	}
	return RC_UNKNOWN_NODE;
}

void ask_node_who_master(ara_cluster_t* cluster, const char* node_name,
	int* has_master, size_t the_master_len, char* the_master) {
	int sock;
	ara_rc rc;
	printf ("Asking %s who is master...\n", node_name);
	rc = open_connection( cluster, node_name, &sock );
	if( rc == RC_SUCCESS ) {
		char req_buf [MXLEN_REQ ];
		size_t req_size = build_req_who_master(MXLEN_REQ, req_buf);
		rc = send_n_bytes(&sock, req_size, req_buf, get_timeout(),MXLEN_ERRMSG, cluster->last_error);
		if( rc == RC_SUCCESS ) {
			rc = check_for_error(&sock,cluster,MXLEN_ERRMSG, cluster->last_error);
			if( rc == RC_SUCCESS ) {
				rc = recv_string_option(&sock,the_master_len,the_master,has_master,MXLEN_ERRMSG, cluster->last_error);
				if(*has_master==1 && strcmp(node_name,the_master) == 0) {
					cluster->master_sock = sock;
				} else {
					close(sock);
				}
				return;
			}
		}
	}
	close(sock);
	(*has_master) = 0;
	return;
}

int determine_master(ara_cluster_t* cluster) {
	ara_node_list_elem_t* iter = cluster->nodes;
	size_t resp_buf_size = MXLEN_NODENAME;
	char resp_buf[resp_buf_size];
	char validation_buf[resp_buf_size];
	int has_master;
	if ( cluster->master_sock != -1 ) {
		close ( cluster->master_sock) ;
		cluster->master_sock = -1;
	}
	while (iter) {
		ask_node_who_master(cluster,iter->cur->name, &has_master, MXLEN_NODENAME, resp_buf);
		if ( has_master == 1) {
			if (cluster->master_sock != - 1) {
				bound_strncpy(cluster->master_name,resp_buf,MXLEN_NODENAME);
				return RC_SUCCESS;
			}
			ask_node_who_master(cluster,resp_buf, &has_master, MXLEN_NODENAME, validation_buf);
			if (has_master == 1 ) {
				if( strcmp(validation_buf, resp_buf) == 0 ) {
					bound_strncpy(cluster->master_name,validation_buf,MXLEN_NODENAME);
					return RC_SUCCESS;
				}
			}
		}

		iter = iter->next;
	}
	bound_strncpy(cluster->last_error, no_master_msg, strlen(no_master_msg));
	return RC_NO_MASTER;
}

int prepare_master_sock (ara_cluster_t* cluster) {
	ara_rc rc = RC_SUCCESS;
	if ( cluster->master_sock == -1) {
		rc = determine_master(cluster);
	}
	return rc;
}

int send_to_master(ara_cluster_t* cluster, size_t req_size, char* req) {
	ara_rc rc;
	rc =  prepare_master_sock(cluster) ;
	if ( rc == RC_SUCCESS ) {
		rc = send_n_bytes(&cluster->master_sock, req_size, req, get_timeout(),MXLEN_ERRMSG,cluster->last_error) ;
	}
	return rc;
}

ara_rc set(ara_cluster_t* cluster, size_t key_size, const char* key, size_t value_size, const char* value) {
	size_t actual_size = build_req_set(MXLEN_REQ, cluster->cmd_buf, key_size, key, value_size, value);
	ara_rc rc;
	rc = send_to_master(cluster, actual_size, cluster->cmd_buf );
	if ( rc == RC_SUCCESS) {
		rc = check_for_error(&cluster->master_sock,cluster,MXLEN_ERRMSG, cluster->last_error);
	}
	return rc;
}


ara_rc who_master(ara_cluster_t* cluster, size_t master_buf_len, char* master_buf) {
	ara_rc rc = RC_SUCCESS;
	if( cluster->master_sock == -1) {
		RET_IF_FAILED( determine_master(cluster) );
	}

	bound_strncpy(master_buf,cluster->master_name,MXLEN_NODENAME);
	return rc;
}

ara_rc exists(ara_cluster_t* cluster, size_t key_size, const char* key, int allow_dirty, int* key_exists ) {
	ara_rc rc ;
	size_t actual_size = build_req_exists(MXLEN_REQ, cluster->cmd_buf, key_size, key, allow_dirty);
	RET_IF_FAILED( send_to_master(cluster,actual_size,cluster->cmd_buf) );
	RET_IF_FAILED( check_for_error(&cluster->master_sock,cluster,MXLEN_ERRMSG, cluster->last_error) );
	RET_IF_FAILED( recv_bool(&cluster->master_sock, key_exists, MXLEN_ERRMSG, cluster->last_error) );
	return rc;
}

ara_rc hello(ara_cluster_t* cluster, size_t client_id_size, const char* client_id, size_t max_response_size, char* response) {
	ara_rc rc;
	size_t actual_size = build_req_hello(MXLEN_REQ,cluster->cmd_buf,
			client_id_size, client_id, cluster->cluster_id_size, cluster->cluster_id) ;
	RET_IF_FAILED( send_to_master(cluster, actual_size, cluster->cmd_buf));
	RET_IF_FAILED( check_for_error(&cluster->master_sock,cluster,MXLEN_ERRMSG, cluster->last_error) );
	RET_IF_FAILED( recv_string(&cluster->master_sock, max_response_size, response, MXLEN_ERRMSG, cluster->last_error) );
	return rc;
}
ara_rc get(ara_cluster_t* cluster, size_t key_size, const char*key, int allow_dirty, size_t max_value_size, char* value) {
	ara_rc rc;
	size_t actual_size = build_req_get(MXLEN_REQ, cluster->cmd_buf, key_size, key, allow_dirty);
	RET_IF_FAILED( send_to_master(cluster,actual_size,cluster->cmd_buf) );
	RET_IF_FAILED( check_for_error(&cluster->master_sock,cluster,MXLEN_ERRMSG, cluster->last_error) );
	RET_IF_FAILED( recv_string(&cluster->master_sock, max_value_size, value, MXLEN_ERRMSG, cluster->last_error) );
	return rc;
}

ara_rc delete(ara_cluster_t* cluster, size_t key_size, const char* key ) {
	ara_rc rc;
	size_t actual_size = build_req_delete(MXLEN_REQ, cluster->cmd_buf, key_size, key);
	RET_IF_FAILED( send_to_master(cluster,actual_size,cluster->cmd_buf) );
	RET_IF_FAILED( check_for_error(&cluster->master_sock,cluster,MXLEN_ERRMSG, cluster->last_error) );
	return rc;
}

int get_last_error( ara_cluster_t* cluster, size_t buf_size, char* buf) {
	bound_strncpy(buf,cluster->last_error,buf_size);
	return RC_SUCCESS;
}

ara_rc range(ara_cluster_t* cluster, size_t b_key_size, const char* b_key, int b_key_included,
		size_t e_key_size, const char* e_key, int e_key_included, int max_cnt, int allow_dirty, key_list_t* key_list)
{
	ara_rc rc;
	size_t actual_size = build_req_range(MXLEN_REQ, cluster->cmd_buf, b_key_size, b_key,
			b_key_included, e_key_size, e_key, e_key_included, max_cnt, allow_dirty);
	RET_IF_FAILED( send_to_master(cluster,actual_size,cluster->cmd_buf) );
	RET_IF_FAILED( check_for_error(&cluster->master_sock,cluster,MXLEN_ERRMSG, cluster->last_error) );
	RET_IF_FAILED( recv_string_list(&cluster->master_sock,key_list,MXLEN_ERRMSG, cluster->last_error));
	return rc;
}

ara_rc range_entries(ara_cluster_t* cluster, size_t b_key_size, const char* b_key, int b_key_included,
		size_t e_key_size, const char* e_key, int e_key_included, int max_cnt, int allow_dirty, kv_pair_list_t* kv_list)
{
    ara_rc rc;
    size_t actual_size = build_req_range_entries(MXLEN_REQ, cluster->cmd_buf, b_key_size, b_key,
                    b_key_included, e_key_size, e_key, e_key_included, max_cnt, allow_dirty);
    RET_IF_FAILED( send_to_master(cluster, actual_size, cluster->cmd_buf) );
    RET_IF_FAILED( check_for_error(&cluster->master_sock, cluster, MXLEN_ERRMSG, cluster->last_error) );
    RET_IF_FAILED( recv_kv_list(&cluster->master_sock, kv_list, MXLEN_ERRMSG, cluster->last_error));
    return rc;
}

ara_rc test_and_set (ara_cluster_t *cluster, size_t key_size, const char* key, size_t old_value_size, const char* old_value,
                size_t new_value_size, const char* new_value, int* is_set, size_t max_return_value_size, char* return_value)
{
    ara_rc rc;
    size_t actual_size = build_req_test_and_set(MXLEN_REQ,cluster->cmd_buf, key_size, key, old_value_size, old_value,
                    new_value_size, new_value);
    RET_IF_FAILED( send_to_master(cluster, actual_size, cluster->cmd_buf) );
    RET_IF_FAILED( check_for_error(&cluster->master_sock, cluster, MXLEN_ERRMSG, cluster->last_error) );
    RET_IF_FAILED( recv_string_option(&cluster->master_sock, max_return_value_size, return_value, is_set, MXLEN_ERRMSG, cluster->last_error) );
    
    return rc;
}

ara_rc multi_get (ara_cluster_t *cluster, key_list_t key_list, int allow_dirty, value_list_t* value_list)
{
    ara_rc rc;
    size_t list_buf_size = get_list_size(key_list);
    if(list_buf_size > MXLEN_REQ)
    {
        cluster->cmd_buf = checked_malloc(list_buf_size * 2);
    }
    size_t actual_size = build_req_multi_get(list_buf_size*2,cluster->cmd_buf, key_list, allow_dirty);
    RET_IF_FAILED( send_to_master(cluster, actual_size, cluster->cmd_buf) );
    RET_IF_FAILED( check_for_error(&cluster->master_sock, cluster, MXLEN_ERRMSG, cluster->last_error) );
    RET_IF_FAILED( recv_string_list(&cluster->master_sock, value_list, MXLEN_ERRMSG, cluster->last_error));

    return rc;
}

ara_rc expect_progress_possible (ara_cluster_t *cluster, int *result)
{
    ara_rc rc;
    size_t actual_size = build_req_expect_progress(MXLEN_REQ,cluster->cmd_buf);
    RET_IF_FAILED( send_to_master(cluster, actual_size, cluster->cmd_buf) );
    RET_IF_FAILED( check_for_error(&cluster->master_sock, cluster, MXLEN_ERRMSG, cluster->last_error) );
    RET_IF_FAILED( recv_bool(&cluster->master_sock, result, MXLEN_ERRMSG, cluster->last_error) );

    return rc;
}

ara_rc sequence (ara_cluster_t *cluster, sequence_t kv_sequence)
{
    ara_rc rc;
    size_t list_buf_size = get_sequence_size(kv_sequence);
    if(list_buf_size > MXLEN_REQ)
    {
        cluster->cmd_buf = checked_malloc(list_buf_size * 2);
    }
    else { 
        list_buf_size = MXLEN_REQ;
    }
        
    size_t actual_size = build_req_sequence(list_buf_size,cluster->cmd_buf, kv_sequence);
    RET_IF_FAILED( send_to_master(cluster, actual_size, cluster->cmd_buf) );
    RET_IF_FAILED( check_for_error(&cluster->master_sock, cluster, MXLEN_ERRMSG, cluster->last_error) );

    return rc;
}


int main(int argc, char** argv) {
	char my_cluster_name [] = "mycluster";
	ara_cluster_t clu = create_empty_cluster( strlen(my_cluster_name), my_cluster_name);
	clu = add_node_to_cluster( clu, "node1", "127.0.0.1", 7080);
	// clu = add_node_to_cluster( clu, "arakoon_1", "127.0.0.1", 7081);
	print_cluster(clu);

	const char key1[] = "keeeyy";
	const char key2[] = "ksfsfdxxxeeeyy";
	const char value1[] = "valllluueue";

	size_t msg_size = 256;
	char msg[msg_size];
	ara_rc rc ;

        /*1. TEST: SET*/
	rc = set(&clu, strlen(key1), key1, strlen(value1), value1);
	if (rc != RC_SUCCESS) {
		puts("ERROR on set");
		get_last_error(&clu, msg_size, msg);
		printf( "%d, %s\n", rc, msg);
	} else {
		puts("SUCCESS on set");
	}

        /*2. TEST: GET*/
	size_t value_size = 256;
	char value [value_size];
	rc = get(&clu, strlen(key1), key1, 0, value_size, value);
	if (rc!= RC_SUCCESS) {
		puts("ERROR on get");
		get_last_error(&clu, value_size, value);
		printf( "%u, %s\n", rc, value );
	} else {
		puts("SUCCESS on get");
		puts(value);
	}

        /*3. TEST: GET with Failure*/
	rc = get(&clu, strlen(key2), key2, 0, value_size, value);
	if (rc!= RC_SUCCESS) {
		puts("ERROR on get2");
		get_last_error(&clu, value_size, value);
		printf( "%u, %s\n", rc, value );
	} else {
		puts("SUCCESS on get2");
		puts(value);
	}

        /*4. TEST: WHO MASTER*/
	rc = who_master(&clu,value_size,value);
	if( rc == RC_SUCCESS ) {
		printf("Found master: %s\n", value) ;
	} else {
		puts("Could not find master got back error:");
		get_last_error(&clu,value_size,value);
		printf ("%u, %s...\n", rc, value);
	}

        /*5. TEST: EXISTS*/
	int is_there;
	rc = exists(&clu, strlen(key2), key2, 0, &is_there) ;
	if (rc!= RC_SUCCESS) {
		puts("ERROR on exists1");
		get_last_error(&clu, value_size, value);
		printf( "%u, %s\n", rc, value );
	} else {
		puts("SUCCESS on exists1");
		printf("%d\n", is_there);
	}

        /*6. TEST: EXISTS*/
	rc = exists(&clu, strlen(key1), key1, 0, &is_there) ;
	if (rc!= RC_SUCCESS) {
		puts("ERROR on exists2");
		get_last_error(&clu, value_size, value);
		printf( "%u, %s\n", rc, value );
	} else {
		puts("SUCCESS on exists2");
		printf("%d\n", is_there);
	}

        /*7. TEST: HELLO*/
	rc = hello(&clu, 4, "meme", value_size, value);
	if (rc!= RC_SUCCESS) {
		puts("ERROR on hello");
		get_last_error(&clu, value_size, value);
		printf( "%u, %s\n", rc, value );
	} else {
		puts("SUCCESS on hello");
		printf("%s\n", value);
	}

        /*8. TEST: DELETE*/
	rc = delete(&clu, strlen(key1), key1);
	if (rc!= RC_SUCCESS) {
		puts("ERROR on delete 1");
		get_last_error(&clu, value_size, value);
		printf( "%u, %s\n", rc, value );
	} else {
		puts("SUCCESS on delete 1");
	}

        /*9. TEST: DELETE with Failure*/
	rc = delete(&clu, strlen(key2), key2);
	if (rc!= RC_SUCCESS) {
		puts("ERROR on delete 2");
		get_last_error(&clu, value_size, value);
		printf( "%u, %s\n", rc, value );
	} else {
		puts("SUCCESS on delete 2");
	}

        /*10. TEST: RANGE*/
	char begin_key [] = "begin_key";
	char end_key [] = "end_key";
	set(&clu, strlen(begin_key),begin_key,strlen(begin_key),begin_key);
	set(&clu, strlen(end_key),end_key,strlen(end_key),end_key);
	key_list_t res_list;
	puts("Starting range");
	rc = range(&clu, strlen(begin_key),begin_key,1,strlen(end_key),end_key,1,-1,0, &res_list);
	if (rc!= RC_SUCCESS) {
		puts("ERROR on range");
		get_last_error(&clu, value_size, value);
		printf( "%u, %s\n", rc, value );
	} else {
		puts("SUCCESS on range");
		printf("Range got back %u elements\n", res_list.count);
		object_list_elem_t iter = res_list.first_ptr;
		while ( iter ) {
			puts( iter->cur.str );
			iter = iter->next;
		}
	}

        /*11. TEST: EXPECT_PROGRESS_POSSIBLE*/
        int progress;
        rc = expect_progress_possible(&clu, &progress);
	if (rc!= RC_SUCCESS) {
		puts("ERROR on Expect Progress Possible");
		get_last_error(&clu, value_size, value);
		printf( "%u, %s\n", rc, value );
	} else {
		puts("SUCCESS on Expect Progress Possible");
		printf("%d\n", progress);
	}

        /*12. TEST: TEST AND SET*/
	const char keytest[] = "keytest";
	const char valueold[] = "old value";
        const char valuenew[] = "new value";
        char valuereturn[12];
        int is_set;
        memset(valuereturn,0,sizeof(valuereturn));
	set(&clu, strlen(keytest),keytest,strlen(valueold),valueold);
        rc = test_and_set(&clu, strlen(keytest), keytest, strlen(valueold), valueold, strlen(valuenew), valuenew, &is_set,
                sizeof(valuereturn), valuereturn);
	if (rc != RC_SUCCESS) {
		puts("ERROR on Test and Set");
		get_last_error(&clu, value_size, value);
		printf( "%u, %s\n", rc, value );
	} else {
            if(strncmp(valuenew,valuereturn,strlen(valuenew))!=0)
            {
                puts("SUCCESS on Test and Set");
                printf("%s\n", valuenew);
            }
            else
            {
                puts("FAILED Test and Set with INVALID return");
            }
	}

        /*13. TEST: TEST AND SET with Failure*/
        memset(valuereturn, 0, sizeof(valuereturn));
	set(&clu, strlen(keytest), keytest, strlen(valueold), valueold);
        rc = test_and_set(&clu, strlen(keytest), keytest, strlen(valuenew), valuenew, strlen(valuenew), valuenew, &is_set,
                sizeof(valuereturn), valuereturn);
	if (rc != RC_SUCCESS) {
		puts("ERROR on Test and Set");
		get_last_error(&clu, value_size, value);
		printf( "%u, %s\n", rc, value );
	} else {
                puts("SUCCESS on Test and Set");
                printf("New: %s\nOld: %s\n", valuenew, valuereturn);
	}

        /*14. TEST: RANGE ENTRIES*/
        kv_pair_list_t kv_list;
	rc = range_entries(&clu, strlen(begin_key), begin_key, 1, strlen(end_key), end_key, 1, -1, 0, &kv_list);
	if (rc!= RC_SUCCESS) {
		puts("ERROR on range entries");
		get_last_error(&clu, value_size, value);
		printf( "%u, %s\n", rc, value );
	} else {
		puts("SUCCESS on range entries");
		printf("Range Entries got back %u elements\n", kv_list.count);
		kv_pair_list_elem_t iter = kv_list.first_ptr;
		while ( iter ) {
                    printf("Key:%s Value:%s\n",iter->cur.key, iter->cur.value);
                    iter = iter->next;
		}
	}

        /*15. TEST: MULTI GET*/
        key_list_t key_list;
        value_list_t value_list;

        int i;
        key_list = init_key_list();
        for( i = 0; i<3; i++)
        {
            object_list_elem_t new_elem = (object_list_elem_t) checked_malloc( sizeof( struct pstring_list_elem )) ;
            char *new_key = checked_malloc(10);
            char new_value[10];
            snprintf(new_key, sizeof(new_key),"Key%d",i);
            snprintf(new_value, sizeof(new_value), "Value%d",i);

            new_elem->cur.size = strlen(new_key);
            new_elem->cur.str = new_key;
            set(&clu, strlen(new_key), new_key, strlen(new_value), new_value);

            new_elem->next = key_list.first_ptr;
            key_list.first_ptr = new_elem;
            key_list.count++;
        }
        printf("Set Done\n");
        
        rc = multi_get(&clu, key_list, 0, &value_list);
	if (rc!= RC_SUCCESS) {
		puts("ERROR on Multi Get");
		get_last_error(&clu, value_size, value);
		printf( "%u, %s\n", rc, value );
	} else {
		puts("SUCCESS on Multi Get");
		printf("Range Entries got back %u elements\n", value_list.count);
		object_list_elem_t iterv = value_list.first_ptr;
                object_list_elem_t iterk = key_list.first_ptr;
		while ( iterv ) {
                    printf("Key:%s Value:%s\n",iterk->cur.str, iterv->cur.str);
                    iterv = iterv->next;
                    iterk = iterk->next;
		}
                cleanup_key_list(&value_list);
	}
        cleanup_key_list(&key_list);

        /*16. TEST: SEQUENCE*/
        sequence_t seq;
        char *seqkey = "KeySeq";

        /*Key to be deleted by sequence*/
        set(&clu, strlen(seqkey), seqkey, strlen(seqkey), seqkey);
        i = 0;
        seq = init_sequence();
        for( i = 0; i<3; i++)
        {
            char *new_key = checked_malloc(10);
            sequence_element_t seq_elem = (sequence_element_t) checked_malloc( sizeof (struct sequence_element) );

            snprintf(new_key, sizeof(new_key), "KeySeq%d",i);
            seq_elem->type = SEQ_SET;
            seq_elem->kv_pair.key_size = strlen(new_key);
            seq_elem->kv_pair.value_size = strlen(new_key);
            seq_elem->kv_pair.key = new_key;
            seq_elem->kv_pair.value = new_key;

            seq_elem->next = seq.first_ptr;
            seq.first_ptr = seq_elem;

            seq.count++;
        }
        sequence_element_t seq_elem = (sequence_element_t) checked_malloc( sizeof (struct sequence_element) );
        seq_elem->type = SEQ_DEL;
        seq_elem->kv_pair.key = seqkey;
        seq_elem->kv_pair.key_size = strlen(seqkey);
        seq_elem->kv_pair.value = NULL;
        seq_elem->kv_pair.value_size = 0;

        seq_elem->next = seq.first_ptr;
        seq.first_ptr = seq_elem;

        seq.count++;

        printf("Set Done\n");

        rc = sequence (&clu, seq);
	if (rc!= RC_SUCCESS) {
		puts("ERROR on Sequence");
		get_last_error(&clu, value_size, value);
		printf( "%u, %s\n", rc, value );
	} else {
		puts("SUCCESS on Sequence");
	}

	cleanup_cluster(&clu);
	return 0;
}


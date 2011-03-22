#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "arakoon_protocol.h"
#include "util.h"

int ARA_VERSION = 1;
char ARA_MAGIC[] = {0xff, 0xb1};

// Hello command
char ARA_CMD_HEL[] 			=  {0x01, 0x00};
char ARA_CMD_WHO[] 			=  {0x02, 0x00};
char ARA_CMD_EXISTS[] 		=  {0x07, 0x00};
char ARA_CMD_GET[] 			=  {0x08, 0x00};
char ARA_CMD_SET[] 			=  {0x09, 0x00};
char ARA_CMD_DEL[] 			=  {0x0a, 0x00};
char ARA_CMD_RAN[] 			=  {0x0b, 0x00};
char ARA_CMD_PRE[] 			=  {0x0c, 0x00};
char ARA_CMD_TAS[] 			=  {0x0d, 0x00};
char ARA_CMD_RAN_E[] 		=  {0x0f, 0x00};
char ARA_CMD_SEQ[] 			=  {0x10, 0x00};
char ARA_CMD_MULTI_GET[] 	=  {0x11, 0x00};
char ARA_CMD_EXP_PROG[] 	=  {0x12, 0x00};
char ARA_CMD_STATS[] 		=  {0x13, 0x00};

void build_opcode( char* buffer, char* operation, size_t* offset) {
	buffer[(*offset)++]=operation[0];
	buffer[(*offset)++]=operation[1];
	buffer[(*offset)++]=ARA_MAGIC[0];
	buffer[(*offset)++]=ARA_MAGIC[1];
}

void add_uint32_to_buf( char* buffer, uint32_t value, size_t* offset) {
	*( (uint32_t*) (buffer+ *offset)) = value ;
	(*offset) += sizeof(uint32_t);
}

void add_int32_to_buf( char* buffer, int32_t value, size_t* offset) {
	*( (int32_t*) (buffer+ *offset)) = value ;
	(*offset) += sizeof(int32_t);
}

void add_buf_to_buf(char* dest, uint32_t src_size, const char* src, size_t* offset)  {
	memcpy(dest+(*offset), src, src_size);
	(*offset) += src_size;
}

void add_bool_to_buf( char* buffer, int val, size_t* offset) {
	if ( val ) {
		buffer[*offset] = 0x01;
		*offset = *offset + 1;
	} else {
		buffer[*offset] = 0x00;
		*offset = *offset + 1;
	}
}

void add_string_to_buf( char* buffer, size_t str_size, const char* str, size_t* offset) {
	add_uint32_to_buf(buffer,str_size,offset);
	add_buf_to_buf(buffer,str_size,str,offset);
}

void add_string_option_to_buf( char* buffer, size_t str_size, const char* str, size_t* offset) {
	if ( str ) {
		buffer[*offset] = 0x01;
		*offset = *offset + 1;
		add_string_to_buf( buffer, str_size, str, offset);
	} else {
		buffer[*offset] = 0x00;
		*offset = *offset + 1;
	}
}

void add_string_list_to_buf (char* buffer, key_list_t key_list, size_t* offset)
{
    object_list_elem_t p = key_list.first_ptr;
    while(p)
    {
        add_string_to_buf(buffer, p->cur.size, p->cur.str, offset);
        p = p->next;
    }
}

void add_string_pair_list_to_buf (char* buffer, kv_pair_list_t key_value_list, size_t* offset)
{
    kv_pair_list_elem_t p = key_value_list.first_ptr;
    while(p)
    {
        add_string_to_buf(buffer, p->cur.key_size, p->cur.key, offset);
        add_string_to_buf(buffer, p->cur.value_size, p->cur.value, offset);
        p = p->next;
    }

}

void check_req_buf_size( size_t required, size_t actual ) {
	if( required > actual ) {
		bail("Request buffer too small");
	}
}

int get_list_size (key_list_t key_list)
{
    int total_key_size = 0;
    struct pstring_list_elem *p = key_list.first_ptr->next;
    total_key_size += key_list.first_ptr->cur.size;
    while(p)
    {
        total_key_size += p->cur.size;
        p = p->next;
    }

    return total_key_size;
}

int get_pair_list_size (kv_pair_list_t key_value_list)
{
    int total_pair_size = 0;
    struct sized_kv_pair_list_elem *p = key_value_list.first_ptr->next;
    total_pair_size += key_value_list.first_ptr->cur.key_size;
    total_pair_size += key_value_list.first_ptr->cur.value_size;
    while(p)
    {
        total_pair_size += p->cur.key_size;
        total_pair_size += p->cur.value_size;
        p = p->next;
    }

    return total_pair_size;
}

size_t get_sequence_size (sequence_t seq)
{
    int total_seq_size = 0;

    sequence_element_t s = seq.first_ptr;
    while(s)
    {
        if(s->type == SEQ_SET)
        {
            total_seq_size += s->kv_pair.key_size;
            total_seq_size += s->kv_pair.value_size;
        }
        else
        {
            total_seq_size += s->kv_pair.key_size;
            total_seq_size += s->kv_pair.value_size;
        }
        s = s->next;
    }

    return total_seq_size;
}

size_t build_req_set ( size_t buf_size, char* buffer,
		uint32_t key_size, const char* key,
		uint32_t value_size, const char* value)
{
	check_req_buf_size( 3*sizeof(uint32_t) + key_size + value_size, buf_size );
	size_t offset = 0;
	build_opcode (buffer, ARA_CMD_SET, &offset);
	add_string_to_buf(buffer, key_size, key, &offset);
	add_string_to_buf(buffer, value_size, value, &offset);
	return offset;
}


size_t build_req_get( size_t buf_size, char* buffer,
		uint32_t key_size, const char* key, int allow_dirty)
{
	check_req_buf_size( 2*sizeof(uint32_t) + key_size + 1, buf_size);
	size_t offset = 0;
	build_opcode (buffer, ARA_CMD_GET, &offset);
	add_bool_to_buf(buffer, allow_dirty, &offset);
	add_string_to_buf(buffer, key_size, key, &offset);
	return offset;
}

size_t build_req_delete( size_t buf_size, char* buffer,
		uint32_t key_size, const char* key)
{
	check_req_buf_size( 2*sizeof(uint32_t) + key_size, buf_size);
	size_t offset = 0;
	build_opcode (buffer, ARA_CMD_DEL, &offset);
	add_string_to_buf(buffer, key_size, key, &offset);
	return offset;
}

size_t build_req_hello( size_t buf_size, char* buffer,
		size_t client_id_size, const char* client_id,
		size_t cluster_id_size, const char* cluster_id)
{
	check_req_buf_size( 3*sizeof(uint32_t) + client_id_size + cluster_id_size, buf_size );
	size_t offset = 0;
	build_opcode(buffer, ARA_CMD_HEL, &offset);
	add_string_to_buf(buffer, client_id_size, client_id, &offset);
	add_string_to_buf(buffer, cluster_id_size, cluster_id, &offset);
	return offset;

}

size_t build_req_exists( size_t buf_size, char* buffer,
		uint32_t key_size, const char* key, int allow_dirty)
{
	check_req_buf_size( 2*sizeof(uint32_t) + key_size + 1, buf_size);
	size_t offset = 0;
	build_opcode(buffer, ARA_CMD_EXISTS, &offset);
	add_bool_to_buf(buffer, allow_dirty, &offset);
	add_string_to_buf(buffer, key_size, key, &offset);
	return offset;
}

size_t build_req_range( size_t buf_size, char* buffer,
		size_t b_key_size, const char* b_key, int b_key_included,
		size_t e_key_size, const char* e_key, int e_key_included,
		int max_cnt, int allow_dirty)
{
	check_req_buf_size( 2 + 3*sizeof(uint32_t) + b_key_size + e_key_size + 1, buf_size );
	size_t offset = 0;
	build_opcode(buffer, ARA_CMD_RAN, &offset);
	add_bool_to_buf(buffer, allow_dirty, &offset);
	add_string_option_to_buf(buffer, b_key_size, b_key, &offset);
	add_bool_to_buf(buffer, b_key_included, &offset);
	add_string_option_to_buf(buffer, e_key_size, e_key, &offset);
	add_bool_to_buf(buffer, e_key_included, &offset);
	add_int32_to_buf(buffer,(int32_t) max_cnt, &offset);
	return offset;
}

size_t build_req_who_master( size_t buf_size, char* buffer) {
	check_req_buf_size( sizeof(uint32_t), buf_size );
	size_t offset = 0;
	build_opcode ( buffer, ARA_CMD_WHO, &offset);
	return offset;
}

ara_rc check_for_error (int* sock, ara_cluster_t* cluster, size_t err_msg_size, char* err_msg) {
	uint32_t response;
	ara_rc rc;
	RET_IF_FAILED( recv_uint32 (sock, &response, err_msg_size, err_msg) ) ;
	if( response != RC_SUCCESS) {
		RET_IF_FAILED( recv_string(sock, MXLEN_ERRMSG, cluster->last_error, err_msg_size, err_msg ) ) ;
	}
	return response;
}

size_t build_req_range_entries( size_t buf_size, char* buffer,
		size_t b_key_size, const char* b_key, int b_key_included,
		size_t e_key_size, const char* e_key, int e_key_included,
                int max_cnt, int allow_dirty)
{
    /*1-byte bool + 1-byte bool + 1-byte bool + 1-int + 1-int + 1-int + 1-int (i.e the combined sizes of the arguments + opcode)*/
    check_req_buf_size( 3 + 4*sizeof(uint32_t) + b_key_size + e_key_size, buf_size );
    size_t offset = 0;
    build_opcode(buffer, ARA_CMD_RAN_E, &offset);
    add_bool_to_buf(buffer, allow_dirty, &offset);
    add_string_option_to_buf(buffer, b_key_size, b_key, &offset);
    add_bool_to_buf(buffer, b_key_included, &offset);
    add_string_option_to_buf(buffer, e_key_size, e_key, &offset);
    add_bool_to_buf(buffer, e_key_included, &offset);
    add_int32_to_buf(buffer,(int32_t) max_cnt, &offset);
    return offset;
}

size_t build_req_test_and_set(size_t buf_size, char* buffer, size_t key_size, const char* key,
                size_t old_value_size , const char* old_value, size_t new_value_size, const char* new_value)
{
    /*1-int + 1-int + 1-int + 1-int(i.e the combined sizes of the arguments + opcode)*/
    check_req_buf_size( 4*sizeof(uint32_t) + key_size + old_value_size + new_value_size, buf_size );
    size_t offset = 0;
    build_opcode(buffer, ARA_CMD_TAS, &offset);
    add_string_to_buf(buffer, key_size, key, &offset);
    add_string_option_to_buf(buffer, old_value_size, old_value, &offset);
    add_string_option_to_buf(buffer, new_value_size, new_value, &offset);

    return offset;
    
}

size_t build_req_multi_get (size_t buf_size, char* buffer, key_list_t key_list, int allow_dirty)
{
    /*1-bool + 1-int + 1-int(i.e the combined sizes of the arguments + opcode)*/
    check_req_buf_size( 1 + 2*sizeof(uint32_t) + get_list_size(key_list), buf_size );
    size_t offset = 0;
    build_opcode(buffer, ARA_CMD_MULTI_GET, &offset);

    add_bool_to_buf(buffer, allow_dirty, &offset);
    add_int32_to_buf(buffer, key_list.count, &offset);
    add_string_list_to_buf(buffer, key_list, &offset);

    return offset;
}

size_t build_req_expect_progress (size_t buf_size, char *buffer)
{
    /*1-int (i.e the combined sizes of the arguments + opcode)*/
    check_req_buf_size( sizeof(uint32_t) , buf_size );
    size_t offset = 0;
    build_opcode(buffer, ARA_CMD_EXP_PROG, &offset);

    return offset;
}

size_t build_req_sequence (size_t buf_size, char* buffer, sequence_t kv_sequence)
{
    char temp_buffer[MXLEN_REQ];
    /*1-int + 1-int(i.e the combined sizes of the arguments + opcode)*/
    check_req_buf_size( 4*sizeof(uint32_t) + get_sequence_size(kv_sequence), buf_size );
    size_t offset = 0;
    build_opcode(buffer, ARA_CMD_SEQ, &offset);

    size_t update_size = build_req_update(MXLEN_REQ, temp_buffer, kv_sequence);

    add_int32_to_buf(buffer, update_size + 2*sizeof(uint32_t), &offset);
    add_int32_to_buf(buffer, 5, &offset);
    add_int32_to_buf(buffer, kv_sequence.count, &offset);
    add_buf_to_buf(buffer,update_size, temp_buffer, &offset);

    return offset;
}

size_t build_req_update(size_t buf_size, char* buffer, sequence_t kv_sequence)
{
    size_t offset = 0;
    sequence_element_t p = kv_sequence.first_ptr;
    while(p)
    {
        if(p->type == SEQ_SET)
        {
            /*SET*/
            add_int32_to_buf (buffer, 1, &offset);
            add_string_to_buf(buffer, p->kv_pair.key_size, p->kv_pair.key, &offset);
            add_string_to_buf(buffer, p->kv_pair.value_size, p->kv_pair.value, &offset);
        }
        else
        {
            /*DELETE*/
            add_int32_to_buf (buffer, 2, &offset);
            add_string_to_buf(buffer, p->kv_pair.key_size, p->kv_pair.key, &offset);
        }
        p = p->next;
    }

    return offset;
}


size_t build_prologue(size_t buffer_size, char* buffer, ara_cluster_t* cluster ) {
    size_t required_size = 3*sizeof(uint32_t) + cluster->cluster_id_size;

    if ( buffer_size >= required_size ) {
            // Magic
            size_t offset = 0;
            buffer[offset++] = 0x00;
            buffer[offset++] = 0x00;
            buffer[offset++] = ARA_MAGIC[0];
            buffer[offset++] = ARA_MAGIC[1];
            // Version
            add_int32_to_buf(buffer, ARA_VERSION, &offset);
            // Cluster
            add_string_to_buf(buffer, cluster->cluster_id_size, cluster->cluster_id, &offset);
    }
    return required_size;
}


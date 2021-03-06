#define ISC_DPB_VERSION1                  1
#define ISC_DPB_CDD_PATHNAME              1
#define ISC_DPB_ALLOCATION                2
#define ISC_DPB_JOURNAL                   3
#define ISC_DPB_PAGE_SIZE                 4
#define ISC_DPB_NUM_BUFFERS               5
#define ISC_DPB_BUFFER_LENGTH             6
#define ISC_DPB_DEBUG                     7
#define ISC_DPB_GARBAGE_COLLECT           8
#define ISC_DPB_VERIFY                    9
#define ISC_DPB_SWEEP                     10
#define ISC_DPB_ENABLE_JOURNAL            11
#define ISC_DPB_DISABLE_JOURNAL           12
#define ISC_DPB_DBKEY_SCOPE               13
#define ISC_DPB_NUMBER_OF_USERS           14
#define ISC_DPB_TRACE                     15
#define ISC_DPB_NO_GARBAGE_COLLECT        16
#define ISC_DPB_DAMAGED                   17
#define ISC_DPB_LICENSE                   18
#define ISC_DPB_SYS_USER_NAME             19
#define ISC_DPB_ENCRYPT_KEY               20
#define ISC_DPB_ACTIVATE_SHADOW           21
#define ISC_DPB_SWEEP_INTERVAL            22
#define ISC_DPB_DELETE_SHADOW             23
#define ISC_DPB_FORCE_WRITE               24
#define ISC_DPB_BEGIN_LOG                 25
#define ISC_DPB_QUIT_LOG                  26
#define ISC_DPB_NO_RESERVE                27
#define ISC_DPB_USER_NAME                 28
#define ISC_DPB_PASSWORD                  29
#define ISC_DPB_PASSWORD_ENC              30
#define ISC_DPB_SYS_USER_NAME_ENC         31
#define ISC_DPB_INTERP                    32
#define ISC_DPB_ONLINE_DUMP               33
#define ISC_DPB_OLD_FILE_SIZE             34
#define ISC_DPB_OLD_NUM_FILES             35
#define ISC_DPB_OLD_FILE                  36
#define ISC_DPB_OLD_START_PAGE            37
#define ISC_DPB_OLD_START_SEQNO           38
#define ISC_DPB_OLD_START_FILE            39
#define ISC_DPB_DROP_WALFILE              40
#define ISC_DPB_OLD_DUMP_ID               41
#define ISC_DPB_WAL_BACKUP_DIR            42
#define ISC_DPB_WAL_CHKPTLEN              43
#define ISC_DPB_WAL_NUMBUFS               44
#define ISC_DPB_WAL_BUFSIZE               45
#define ISC_DPB_WAL_GRP_CMT_WAIT          46
#define ISC_DPB_LC_MESSAGES               47
#define ISC_DPB_LC_CTYPE                  48
#define ISC_DPB_CACHE_MANAGER             49
#define ISC_DPB_SHUTDOWN                  50
#define ISC_DPB_ONLINE                    51
#define ISC_DPB_SHUTDOWN_DELAY            52
#define ISC_DPB_RESERVED                  53
#define ISC_DPB_OVERWRITE                 54
#define ISC_DPB_SEC_ATTACH                55
#define ISC_DPB_DISABLE_WAL               56
#define ISC_DPB_CONNECT_TIMEOUT           57
#define ISC_DPB_DUMMY_PACKET_INTERVAL     58
#define ISC_DPB_GBAK_ATTACH               59
#define ISC_DPB_SQL_ROLE_NAME             60
#define ISC_DPB_SET_PAGE_BUFFERS          61
#define ISC_DPB_WORKING_DIRECTORY         62
#define ISC_DPB_SQL_DIALECT               63
#define ISC_DPB_SET_DB_READONLY           64
#define ISC_DPB_SET_DB_SQL_DIALECT        65
#define ISC_DPB_GFIX_ATTACH               66
#define ISC_DPB_GSTAT_ATTACH              67
#define ISC_DPB_SET_DB_CHARSET            68
#define ISC_DPB_GSEC_ATTACH               69
#define ISC_DPB_ADDRESS_PATH              70
#define ISC_DPB_PROCESS_ID                71
#define ISC_DPB_NO_DB_TRIGGERS            72
#define ISC_DPB_TRUSTED_AUTH              73
#define ISC_DPB_PROCESS_NAME              74
#define ISC_DPB_TRUSTED_ROLE              75
#define ISC_DPB_ORG_FILENAME              76
#define ISC_DPB_UTF8_FILENAME             77
#define ISC_DPB_EXT_CALL_DEPTH            78

#define ISC_TPB_VERSION1                  1
#define ISC_TPB_VERSION3                  3
#define ISC_TPB_CONSISTENCY               1
#define ISC_TPB_CONCURRENCY               2
#define ISC_TPB_SHARED                    3
#define ISC_TPB_PROTECTED                 4
#define ISC_TPB_EXCLUSIVE                 5
#define ISC_TPB_WAIT                      6
#define ISC_TPB_NOWAIT                    7
#define ISC_TPB_READ                      8
#define ISC_TPB_WRITE                     9
#define ISC_TPB_LOCK_READ                 10
#define ISC_TPB_LOCK_WRITE                11
#define ISC_TPB_VERB_TIME                 12
#define ISC_TPB_COMMIT_TIME               13
#define ISC_TPB_IGNORE_LIMBO              14
#define ISC_TPB_READ_COMMITTED            15
#define ISC_TPB_AUTOCOMMIT                16
#define ISC_TPB_REC_VERSION               17
#define ISC_TPB_NO_REC_VERSION            18
#define ISC_TPB_RESTART_REQUESTS          19
#define ISC_TPB_NO_AUTO_UNDO              20
#define ISC_TPB_LOCK_TIMEOUT              21

#define ISC_INFO_SQL_STMT_SELECT          1
#define ISC_INFO_SQL_STMT_INSERT          2
#define ISC_INFO_SQL_STMT_UPDATE          3
#define ISC_INFO_SQL_STMT_DELETE          4
#define ISC_INFO_SQL_STMT_DDL             5
#define ISC_INFO_SQL_STMT_GET_SEGMENT     6
#define ISC_INFO_SQL_STMT_PUT_SEGMENT     7
#define ISC_INFO_SQL_STMT_EXEC_PROCEDURE  8
#define ISC_INFO_SQL_STMT_START_TRANS     9
#define ISC_INFO_SQL_STMT_COMMIT          10
#define ISC_INFO_SQL_STMT_ROLLBACK        11
#define ISC_INFO_SQL_STMT_SELECT_FOR_UPD  12
#define ISC_INFO_SQL_STMT_SET_GENERATOR   13
#define ISC_INFO_SQL_STMT_SAVEPOINT       14

#define SQL_TEXT                           452
#define SQL_VARYING                        448
#define SQL_SHORT                          500
#define SQL_LONG                           496
#define SQL_FLOAT                          482
#define SQL_DOUBLE                         480
#define SQL_D_FLOAT                        530
#define SQL_TIMESTAMP                      510
#define SQL_BLOB                           520
#define SQL_ARRAY                          540
#define SQL_QUAD                           550
#define SQL_TYPE_TIME                      560
#define SQL_TYPE_DATE                      570
#define SQL_INT64                          580
#define SQL_NULL                         32766
#define SQL_DATE                           SQL_TIMESTAMP

#define ISC_SPB_VERSION1                  1
#define ISC_SPB_CURRENT_VERSION           2
#define ISC_SPB_VERSION                   ISC_SPB_CURRENT_VERSION
#define ISC_SPB_USER_NAME                 ISC_DPB_USER_NAME
#define ISC_SPB_SYS_USER_NAME             ISC_DPB_SYS_USER_NAME
#define ISC_SPB_SYS_USER_NAME_ENC         ISC_DPB_SYS_USER_NAME_ENC
#define ISC_SPB_PASSWORD                  ISC_DPB_PASSWORD
#define ISC_SPB_PASSWORD_ENC              ISC_DPB_PASSWORD_ENC
#define ISC_SPB_COMMAND_LINE              105
#define ISC_SPB_DBNAME                    106
#define ISC_SPB_VERBOSE                   107
#define ISC_SPB_OPTIONS                   108
#define ISC_SPB_ADDRESS_PATH              109
#define ISC_SPB_PROCESS_ID                110
#define ISC_SPB_TRUSTED_AUTH                      111
#define ISC_SPB_PROCESS_NAME              112
#define ISC_SPB_TRUSTED_ROLE              113

#define ISC_SPB_CONNECT_TIMEOUT           ISC_DPB_CONNECT_TIMEOUT
#define ISC_SPB_DUMMY_PACKET_INTERVAL     ISC_DPB_DUMMY_PACKET_INTERVAL
#define ISC_SPB_SQL_ROLE_NAME             ISC_DPB_SQL_ROLE_NAME

#define ISC_ACTION_SVC_BACKUP          1
#define ISC_ACTION_SVC_RESTORE         2
#define ISC_ACTION_SVC_REPAIR          3
#define ISC_ACTION_SVC_ADD_USER        4
#define ISC_ACTION_SVC_DELETE_USER     5
#define ISC_ACTION_SVC_MODIFY_USER     6
#define ISC_ACTION_SVC_DISPLAY_USER    7
#define ISC_ACTION_SVC_PROPERTIES      8
#define ISC_ACTION_SVC_ADD_LICENSE     9
#define ISC_ACTION_SVC_REMOVE_LICENSE 10
#define ISC_ACTION_SVC_DB_STATS       11
#define ISC_ACTION_SVC_GET_IB_LOG     12
#define ISC_ACTION_SVC_GET_FB_LOG     12
#define ISC_ACTION_SVC_NBAK           20
#define ISC_ACTION_SVC_NREST          21
#define ISC_ACTION_SVC_TRACE_START    22
#define ISC_ACTION_SVC_TRACE_STOP     23
#define ISC_ACTION_SVC_TRACE_SUSPEND  24
#define ISC_ACTION_SVC_TRACE_RESUME   25
#define ISC_ACTION_SVC_TRACE_LIST     26
#define ISC_ACTION_SVC_SET_MAPPING    27
#define ISC_ACTION_SVC_DROP_MAPPING   28
#define ISC_ACTION_SVC_DISPLAY_USER_ADM 29
#define ISC_ACTION_SVC_VALIDATE           30
#define ISC_ACTION_SVC_LAST                       31

#define ISC_INFO_SVC_SVR_DB_INFO      50
#define ISC_INFO_SVC_GET_LICENSE      51
#define ISC_INFO_SVC_GET_LICENSE_MASK 52
#define ISC_INFO_SVC_GET_CONFIG       53
#define ISC_INFO_SVC_VERSION          54
#define ISC_INFO_SVC_SERVER_VERSION   55
#define ISC_INFO_SVC_IMPLEMENTATION   56
#define ISC_INFO_SVC_CAPABILITIES     57
#define ISC_INFO_SVC_USER_DBPATH      58
#define ISC_INFO_SVC_GET_ENV          59
#define ISC_INFO_SVC_GET_ENV_LOCK     60
#define ISC_INFO_SVC_GET_ENV_MSG      61
#define ISC_INFO_SVC_LINE             62
#define ISC_INFO_SVC_TO_EOF           63
#define ISC_INFO_SVC_TIMEOUT          64
#define ISC_INFO_SVC_GET_LICENSED_USERS 65
#define ISC_INFO_SVC_LIMBO_TRANS        66
#define ISC_INFO_SVC_RUNNING            67
#define ISC_INFO_SVC_GET_USERS          68
#define ISC_INFO_SVC_STDIN                      78

#define ISC_SPB_SEC_USERID            5
#define ISC_SPB_SEC_GROUPID           6
#define ISC_SPB_SEC_USERNAME          7
#define ISC_SPB_SEC_PASSWORD          8
#define ISC_SPB_SEC_GROUPNAME         9
#define ISC_SPB_SEC_FIRSTNAME         10
#define ISC_SPB_SEC_MIDDLENAME        11
#define ISC_SPB_SEC_LASTNAME          12
#define ISC_SPB_SEC_ADMIN             13

#define ISC_SPB_LIC_KEY               5
#define ISC_SPB_LIC_ID                6
#define ISC_SPB_LIC_DESC              7

#define ISC_SPB_BKP_FILE                 5
#define ISC_SPB_BKP_FACTOR               6
#define ISC_SPB_BKP_LENGTH               7
#define ISC_SPB_BKP_STAT                 15
#define ISC_SPB_BKP_IGNORE_CHECKSUMS     0x01
#define ISC_SPB_BKP_IGNORE_LIMBO         0x02
#define ISC_SPB_BKP_METADATA_ONLY        0x04
#define ISC_SPB_BKP_NO_GARBAGE_COLLECT   0x08
#define ISC_SPB_BKP_OLD_DESCRIPTIONS     0x10
#define ISC_SPB_BKP_NON_TRANSPORTABLE    0x20
#define ISC_SPB_BKP_CONVERT              0x40
#define ISC_SPB_BKP_EXPAND                               0x80
#define ISC_SPB_BKP_NO_TRIGGERS                  0x8000

#define ISC_SPB_PRP_PAGE_BUFFERS                5
#define ISC_SPB_PRP_SWEEP_INTERVAL              6
#define ISC_SPB_PRP_SHUTDOWN_DB                 7
#define ISC_SPB_PRP_DENY_NEW_ATTACHMENTS        9
#define ISC_SPB_PRP_DENY_NEW_TRANSACTIONS       10
#define ISC_SPB_PRP_RESERVE_SPACE               11
#define ISC_SPB_PRP_WRITE_MODE                  12
#define ISC_SPB_PRP_ACCESS_MODE                 13
#define ISC_SPB_PRP_SET_SQL_DIALECT             14
#define ISC_SPB_PRP_ACTIVATE                    0x0100
#define ISC_SPB_PRP_DB_ONLINE                   0x0200
#define ISC_SPB_PRP_FORCE_SHUTDOWN                      41
#define ISC_SPB_PRP_ATTACHMENTS_SHUTDOWN        42
#define ISC_SPB_PRP_TRANSACTIONS_SHUTDOWN       43
#define ISC_SPB_PRP_SHUTDOWN_MODE               44
#define ISC_SPB_PRP_ONLINE_MODE                 45

#define ISC_SPB_PRP_SM_NORMAL           0
#define ISC_SPB_PRP_SM_MULTI            1
#define ISC_SPB_PRP_SM_SINGLE           2
#define ISC_SPB_PRP_SM_FULL                     3

#define ISC_SPB_PRP_RES_USE_FULL        35
#define ISC_SPB_PRP_RES                         36

#define ISC_SPB_PRP_WM_ASYNC            37
#define ISC_SPB_PRP_WM_SYNC                     38

#define ISC_SPB_PRP_AM_READONLY         39
#define ISC_SPB_PRP_AM_READWRITE        40

#define ISC_SPB_RPR_COMMIT_TRANS                15
#define ISC_SPB_RPR_ROLLBACK_TRANS              34
#define ISC_SPB_RPR_RECOVER_TWO_PHASE   17
#define ISC_SPB_TRA_ID                                  18
#define ISC_SPB_SINGLE_TRA_ID                   19
#define ISC_SPB_MULTI_TRA_ID                    20
#define ISC_SPB_TRA_STATE                               21
#define ISC_SPB_TRA_STATE_LIMBO                 22
#define ISC_SPB_TRA_STATE_COMMIT                23
#define ISC_SPB_TRA_STATE_ROLLBACK              24
#define ISC_SPB_TRA_STATE_UNKNOWN               25
#define ISC_SPB_TRA_HOST_SITE                   26
#define ISC_SPB_TRA_REMOTE_SITE                 27
#define ISC_SPB_TRA_DB_PATH                             28
#define ISC_SPB_TRA_ADVISE                              29
#define ISC_SPB_TRA_ADVISE_COMMIT               30
#define ISC_SPB_TRA_ADVISE_ROLLBACK             31
#define ISC_SPB_TRA_ADVISE_UNKNOWN              33

#define ISC_SPB_RPR_VALIDATE_DB                 0x01
#define ISC_SPB_RPR_SWEEP_DB                    0x02
#define ISC_SPB_RPR_MEND_DB                             0x04
#define ISC_SPB_RPR_LIST_LIMBO_TRANS    0x08
#define ISC_SPB_RPR_CHECK_DB                    0x10
#define ISC_SPB_RPR_IGNORE_CHECKSUM             0x20
#define ISC_SPB_RPR_KILL_SHADOWS                0x40
#define ISC_SPB_RPR_FULL                                0x80

#define ISC_SPB_RES_BUFFERS                             9
#define ISC_SPB_RES_PAGE_SIZE                   10
#define ISC_SPB_RES_LENGTH                              11
#define ISC_SPB_RES_ACCESS_MODE                 12
#define ISC_SPB_RES_FIX_FSS_DATA                13
#define ISC_SPB_RES_FIX_FSS_METADATA    14
#define ISC_SPB_RES_STAT                                ISC_SPB_BKP_STAT
#define ISC_SPB_RES_METADATA_ONLY               ISC_SPB_BKP_METADATA_ONLY
#define ISC_SPB_RES_DEACTIVATE_IDX              0x0100
#define ISC_SPB_RES_NO_SHADOW                   0x0200
#define ISC_SPB_RES_NO_VALIDITY                 0x0400
#define ISC_SPB_RES_ONE_AT_A_TIME               0x0800
#define ISC_SPB_RES_REPLACE                             0x1000
#define ISC_SPB_RES_CREATE                              0x2000
#define ISC_SPB_RES_USE_ALL_SPACE               0x4000

#define ISC_SPB_VAL_TAB_INCL            1
#define ISC_SPB_VAL_TAB_EXCL            2
#define ISC_SPB_VAL_IDX_INCL            3
#define ISC_SPB_VAL_IDX_EXCL            4
#define ISC_SPB_VAL_LOCK_TIMEOUT        5

#define ISC_SPB_RES_AM_READONLY                 ISC_SPB_PRP_AM_READONLY
#define ISC_SPB_RES_AM_READWRITE                ISC_SPB_PRP_AM_READWRITE

#define ISC_SPB_NUM_ATT                 5
#define ISC_SPB_NUM_DB                  6

#define ISC_SPB_STS_DATA_PAGES          0x01
#define ISC_SPB_STS_DB_LOG                      0x02
#define ISC_SPB_STS_HDR_PAGES           0x04
#define ISC_SPB_STS_IDX_PAGES           0x08
#define ISC_SPB_STS_SYS_RELATIONS       0x10
#define ISC_SPB_STS_RECORD_VERSIONS     0x20
#define ISC_SPB_STS_TABLE                       0x40
#define ISC_SPB_STS_NOCREATION          0x80

#define ISC_SPB_NBK_LEVEL                       5
#define ISC_SPB_NBK_FILE                        6
#define ISC_SPB_NBK_DIRECT                      7
#define ISC_SPB_NBK_NO_TRIGGERS         0x01

#define ISC_SPB_TRC_ID                          1
#define ISC_SPB_TRC_NAME                        2
#define ISC_SPB_TRC_CFG                         3

#define ISC_INFO_END                    1
#define ISC_INFO_TRUNCATED              2
#define ISC_INFO_ERROR                  3
#define ISC_INFO_DATA_NOT_READY           4
#define ISC_INFO_LENGTH                 126
#define ISC_INFO_FLAG_END               127

#define ISC_INFO_DB_ID                  4
#define ISC_INFO_READS                  5
#define ISC_INFO_WRITES                 6
#define ISC_INFO_FETCHES                7
#define ISC_INFO_MARKS                  8
#define ISC_INFO_IMPLEMENTATION         11
#define ISC_INFO_ISC_VERSION            12
#define ISC_INFO_BASE_LEVEL             13
#define ISC_INFO_PAGE_SIZE              14
#define ISC_INFO_NUM_BUFFERS            15
#define ISC_INFO_LIMBO                  16
#define ISC_INFO_CURRENT_MEMORY         17
#define ISC_INFO_MAX_MEMORY             18
#define ISC_INFO_WINDOW_TURNS           19
#define ISC_INFO_LICENSE                20
#define ISC_INFO_ALLOCATION             21
#define ISC_INFO_ATTACHMENT_ID          22
#define ISC_INFO_READ_SEQ_COUNT         23
#define ISC_INFO_READ_IDX_COUNT         24
#define ISC_INFO_INSERT_COUNT           25
#define ISC_INFO_UPDATE_COUNT           26
#define ISC_INFO_DELETE_COUNT           27
#define ISC_INFO_BACKOUT_COUNT          28
#define ISC_INFO_PURGE_COUNT            29
#define ISC_INFO_EXPUNGE_COUNT          30
#define ISC_INFO_SWEEP_INTERVAL         31
#define ISC_INFO_ODS_VERSION            32
#define ISC_INFO_ODS_MINOR_VERSION      33
#define ISC_INFO_NO_RESERVE             34
#define ISC_INFO_LOGFILE                35
#define ISC_INFO_CUR_LOGFILE_NAME       36
#define ISC_INFO_CUR_LOG_PART_OFFSET    37
#define ISC_INFO_NUM_WAL_BUFFERS        38
#define ISC_INFO_WAL_BUFFER_SIZE        39
#define ISC_INFO_WAL_CKPT_LENGTH        40
#define ISC_INFO_WAL_CUR_CKPT_INTERVAL  41
#define ISC_INFO_WAL_PRV_CKPT_FNAME     42
#define ISC_INFO_WAL_PRV_CKPT_POFFSET   43
#define ISC_INFO_WAL_RECV_CKPT_FNAME    44
#define ISC_INFO_WAL_RECV_CKPT_POFFSET  45
#define ISC_INFO_WAL_GRPC_WAIT_USECS    47
#define ISC_INFO_WAL_NUM_IO             48
#define ISC_INFO_WAL_AVG_IO_SIZE        49
#define ISC_INFO_WAL_NUM_COMMITS        50
#define ISC_INFO_WAL_AVG_GRPC_SIZE      51
#define ISC_INFO_FORCED_WRITES          52
#define ISC_INFO_USER_NAMES             53
#define ISC_INFO_PAGE_ERRORS            54
#define ISC_INFO_RECORD_ERRORS          55
#define ISC_INFO_BPAGE_ERRORS           56
#define ISC_INFO_DPAGE_ERRORS           57
#define ISC_INFO_IPAGE_ERRORS           58
#define ISC_INFO_PPAGE_ERRORS           59
#define ISC_INFO_TPAGE_ERRORS           60
#define ISC_INFO_SET_PAGE_BUFFERS       61
#define ISC_INFO_DB_SQL_DIALECT         62
#define ISC_INFO_DB_READ_ONLY           63
#define ISC_INFO_DB_SIZE_IN_PAGES       64
#define FRB_INFO_ATT_CHARSET            101
#define ISC_INFO_DB_CLASS               102
#define ISC_INFO_FIREBIRD_VERSION       103
#define ISC_INFO_OLDEST_TRANSACTION     104
#define ISC_INFO_OLDEST_ACTIVE          105
#define ISC_INFO_OLDEST_SNAPSHOT        106
#define ISC_INFO_NEXT_TRANSACTION       107
#define ISC_INFO_DB_PROVIDER            108
#define ISC_INFO_ACTIVE_TRANSACTIONS    109
#define ISC_INFO_ACTIVE_TRAN_COUNT      110
#define ISC_INFO_CREATION_DATE          111
#define ISC_INFO_DB_FILE_SIZE           112
#define FB_INFO_PAGE_CONTENTS           113



/* dataset states */
#define FBDS_INACTIVE                     0
#define FBDS_BROWSE                       1
#define FBDS_EDIT                         2
#define FBDS_APPEND                       3

/* transaction DEFAULT action and event info */
#define FBTA_START                        1
#define FBTA_COMMIT                       2
#define FBTA_COMMITRETAINING              3
#define FBTA_ROLLBACK                     4
#define FBTA_ROLLBACKRETAINING            5

/* field structure */
#define FBFS_NAME                         1
#define FBFS_TYPE                         2
#define FBFS_LENGTH                       3
#define FBFS_SCALE                        4
#define FBFS_RELNAME                      5
#define FBFS_ALIASNAME_LEN                6
#define FBFS_ALIASNAME                    7

/* generator - when apply  */
#define FBGW_ON_NEW_RECORD                1
#define FBGW_ON_POST                      2
#define FBGW_ON_SERVER                    3

typedef enum
    { DOUBLE = 0
    , INT    = 1
    , BOOL   = 2
    } scalar;

typedef struct array_data {
    scalar *type;
    void *data;
};

typedef struct array {
    int *shape;
    array_data *data;
};

// typedef struct accel_type {
//     type_tag tag;
//     union {
//         accel_tuple *tuple;
//         accel_scalar scalar;
//     } type;
// };


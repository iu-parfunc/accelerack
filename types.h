
typedef enum
    { DOUBLE = 0
    , INT    = 1
    , BOOL   = 2
    } scalar;

typedef enum
  { SCALAR = 0
  , TUPLE  = 1
  } type_tag;

typedef struct accel_tuple {
    int tuple_length;
    type *tuple_types;
};

typedef struct accel_type {
    type_tag tag;
    union {
        accel_tuple *tuple;
        accel_scalar scalar;
    } type;
};


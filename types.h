
typedef enum
    { DOUBLE = 0
    , INT    = 1
    , BOOL   = 2
    } scalar;

typedef struct array_data {
    scalar type;
    void *data;
};

typedef struct array {
    int shape_length;
    int *shape;
    int data_length;
    array_data *data;
};


MODULE buscador
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: TokenRegistry, create_registry, add_token
    PUBLIC :: modify_token, get_token_info, remove_token
    PUBLIC :: display_tokens, token_exists

    ! Module constants
    INTEGER, PARAMETER :: STR_SHORT = 50
    INTEGER, PARAMETER :: STR_MEDIUM = 100
    INTEGER, PARAMETER :: STR_LONG = 250
    INTEGER, PARAMETER :: INITIAL_CAPACITY = 8

    ! Token position type
    TYPE :: Position
        CHARACTER(STR_SHORT) :: x = ''
        CHARACTER(STR_SHORT) :: y = ''
    END TYPE Position

    ! Token metadata type
    TYPE :: TokenMetadata
        CHARACTER(STR_SHORT) :: creation_date = ''
        CHARACTER(STR_SHORT) :: last_modified = ''
        LOGICAL :: is_active = .TRUE.
    END TYPE TokenMetadata

    ! Main token type
    TYPE :: TokenData
        CHARACTER(STR_MEDIUM) :: id = ''
        CHARACTER(STR_SHORT) :: class_type = ''
        CHARACTER(STR_LONG) :: content = ''
        CHARACTER(STR_MEDIUM) :: collection_id = ''
        TYPE(Position) :: pos
        TYPE(TokenMetadata) :: metadata
    CONTAINS
        PROCEDURE :: init => initialize_token
        PROCEDURE :: update => update_token_data
    END TYPE TokenData

    ! Registry container type
    TYPE :: TokenRegistry
        PRIVATE
        TYPE(TokenData), ALLOCATABLE :: tokens(:)
        INTEGER :: count = 0
        INTEGER :: max_size = 0
    CONTAINS
        PROCEDURE :: initialize => initialize_registry
        PROCEDURE :: expand => expand_registry
        PROCEDURE :: add => add_token_internal
        PROCEDURE :: find => find_token
        PROCEDURE :: remove => remove_token_internal
    END TYPE TokenRegistry

CONTAINS

    ! === TokenData Methods ===
    SUBROUTINE initialize_token(this, id, class_type)
        CLASS(TokenData), INTENT(INOUT) :: this
        CHARACTER(*), INTENT(IN) :: id, class_type
        
        this%id = id
        this%class_type = class_type
        this%content = ''
        this%collection_id = ''
        this%metadata%is_active = .TRUE.
        this%metadata%creation_date = get_current_timestamp()
        this%metadata%last_modified = this%metadata%creation_date
    END SUBROUTINE initialize_token

    SUBROUTINE update_token_data(this, content, collection_id, x, y)
        CLASS(TokenData), INTENT(INOUT) :: this
        CHARACTER(*), INTENT(IN), OPTIONAL :: content, collection_id, x, y
        
        IF (PRESENT(content)) this%content = content
        IF (PRESENT(collection_id)) this%collection_id = collection_id
        IF (PRESENT(x)) this%pos%x = x
        IF (PRESENT(y)) this%pos%y = y
        this%metadata%last_modified = get_current_timestamp()
    END SUBROUTINE update_token_data

    ! === TokenRegistry Methods ===
    SUBROUTINE initialize_registry(this)
        CLASS(TokenRegistry), INTENT(INOUT) :: this
        
        this%max_size = INITIAL_CAPACITY
        this%count = 0
        ALLOCATE(this%tokens(this%max_size))
    END SUBROUTINE initialize_registry

    SUBROUTINE expand_registry(this)
        CLASS(TokenRegistry), INTENT(INOUT) :: this
        TYPE(TokenData), ALLOCATABLE :: temp(:)
        INTEGER :: new_size
        
        new_size = this%max_size * 2
        ALLOCATE(temp(new_size))
        temp(1:this%count) = this%tokens(1:this%count)
        
        DEALLOCATE(this%tokens)
        ALLOCATE(this%tokens(new_size))
        this%tokens = temp
        this%max_size = new_size
        
        DEALLOCATE(temp)
    END SUBROUTINE expand_registry

    FUNCTION find_token(this, id) RESULT(idx)
        CLASS(TokenRegistry), INTENT(IN) :: this
        CHARACTER(*), INTENT(IN) :: id
        INTEGER :: idx, i
        
        idx = -1
        DO i = 1, this%count
            IF (TRIM(this%tokens(i)%id) == TRIM(id)) THEN
                idx = i
                EXIT
            END IF
        END DO
    END FUNCTION find_token

    ! === Public Interface ===
    FUNCTION create_registry() RESULT(registry)
        TYPE(TokenRegistry) :: registry
        CALL registry%initialize()
    END FUNCTION create_registry

    SUBROUTINE add_token(registry, id, class_type, content, collection_id, x, y)
        TYPE(TokenRegistry), INTENT(INOUT) :: registry
        CHARACTER(*), INTENT(IN) :: id, class_type
        CHARACTER(*), INTENT(IN), OPTIONAL :: content, collection_id, x, y
        TYPE(TokenData) :: new_token
        
        IF (registry%find(id) > 0) THEN
            PRINT *, "Warning: Token ID already exists:", TRIM(id)
            RETURN
        END IF
        
        CALL new_token%init(id, class_type)
        IF (PRESENT(content)) CALL new_token%update(content=content)
        IF (PRESENT(collection_id)) CALL new_token%update(collection_id=collection_id)
        IF (PRESENT(x) .AND. PRESENT(y)) CALL new_token%update(x=x, y=y)
        
        CALL registry%add_token_internal(new_token)
    END SUBROUTINE add_token

    SUBROUTINE add_token_internal(this, token)
        CLASS(TokenRegistry), INTENT(INOUT) :: this
        TYPE(TokenData), INTENT(IN) :: token
        
        IF (this%count == this%max_size) CALL this%expand()
        
        this%count = this%count + 1
        this%tokens(this%count) = token
    END SUBROUTINE add_token_internal

    SUBROUTINE modify_token(registry, id, content, collection_id, x, y)
        TYPE(TokenRegistry), INTENT(INOUT) :: registry
        CHARACTER(*), INTENT(IN) :: id
        CHARACTER(*), INTENT(IN), OPTIONAL :: content, collection_id, x, y
        INTEGER :: idx
        
        idx = registry%find(id)
        IF (idx > 0) THEN
            CALL registry%tokens(idx)%update(content, collection_id, x, y)
        END IF
    END SUBROUTINE modify_token

    FUNCTION get_token_info(registry, id) RESULT(token)
        TYPE(TokenRegistry), INTENT(IN) :: registry
        CHARACTER(*), INTENT(IN) :: id
        TYPE(TokenData) :: token
        INTEGER :: idx
        
        idx = registry%find(id)
        IF (idx > 0) token = registry%tokens(idx)
    END FUNCTION get_token_info

    SUBROUTINE display_tokens(registry)
        TYPE(TokenRegistry), INTENT(IN) :: registry
        INTEGER :: i
        
        IF (registry%count == 0) THEN
            PRINT *, "Registry is empty"
            RETURN
        END IF
        
        DO i = 1, registry%count
            PRINT *, "=== Token", i, "==="
            PRINT *, "ID:", TRIM(registry%tokens(i)%id)
            PRINT *, "Type:", TRIM(registry%tokens(i)%class_type)
            PRINT *, "Content:", TRIM(registry%tokens(i)%content)
            PRINT *, "Collection:", TRIM(registry%tokens(i)%collection_id)
            PRINT *, "Position:", TRIM(registry%tokens(i)%pos%x), ",", &
                    TRIM(registry%tokens(i)%pos%y)
            PRINT *, "Active:", registry%tokens(i)%metadata%is_active
            PRINT *, "Last Modified:", TRIM(registry%tokens(i)%metadata%last_modified)
            PRINT *, "=============="
        END DO
    END SUBROUTINE display_tokens

    FUNCTION token_exists(registry, id) RESULT(exists)
        TYPE(TokenRegistry), INTENT(IN) :: registry
        CHARACTER(*), INTENT(IN) :: id
        LOGICAL :: exists
        
        exists = registry%find(id) > 0
    END FUNCTION token_exists

    SUBROUTINE remove_token(registry, id)
        TYPE(TokenRegistry), INTENT(INOUT) :: registry
        CHARACTER(*), INTENT(IN) :: id
        
        CALL registry%remove(id)
    END SUBROUTINE remove_token

    SUBROUTINE remove_token_internal(this, id)
        CLASS(TokenRegistry), INTENT(INOUT) :: this
        CHARACTER(*), INTENT(IN) :: id
        INTEGER :: idx, i
        
        idx = this%find(id)
        IF (idx > 0) THEN
            DO i = idx, this%count - 1
                this%tokens(i) = this%tokens(i + 1)
            END DO
            this%count = this%count - 1
        END IF
    END SUBROUTINE remove_token_internal

    ! === Utility Functions ===
    FUNCTION get_current_timestamp() RESULT(timestamp)
        CHARACTER(STR_SHORT) :: timestamp
        INTEGER :: values(8)
        
        CALL DATE_AND_TIME(VALUES=values)
        WRITE(timestamp, '(I4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2)') &
              values(1), values(2), values(3), values(5), values(6), values(7)
    END FUNCTION get_current_timestamp

END MODULE buscador
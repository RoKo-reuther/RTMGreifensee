
SUBROUTINE setup_grid (a, b, N, xmid, xint, dx, dxaux)

    IMPLICIT NONE
  
    !----- INPUT PARAMETERS -----

    ! Start (a) and end (b) of grid range
    REAL(kind = 8)           :: a
    REAL(kind = 8)           :: b

    ! Number of grid layers
    INTEGER                  :: N

    !----- VARIABLES TO CHANGE ------

    ! Grid cell centers
    REAL(kind = 8)           :: xmid(N)

    ! Grid cell interfaces
    REAL(kind = 8)           :: xint(N + 1)

    ! Distance between grid cell interfaces
    REAL(kind = 8)           :: dx(N)

    ! Distance between grid cell centers + boundary interfaces
    REAL(kind = 8)           :: dxaux(N + 1)

    !----- LOCAL VARIABLES -----

    REAL(kind = 8)           :: mida, midb
    INTEGER                  :: i

    !--------------------------------------------------------------------------

    dx         = (b - a) / N
    xint       = [(i*dx(1), i=0,N, 1)]
    mida       = a + 0.5 * dx(1)
    midb       = b - 0.5 * dx(1)
    xmid       = [(mida + i*dx(1), i=0,N-1, 1)]
    dxaux(1)   = 0.5 * dx(1)
    dxaux(2:N) = dx(1)
    dxaux(N+1) = 0.5 * dx(1)
  
    return
  
END SUBROUTINE setup_grid


SUBROUTINE pexp (x, y_0, y_inf, x_att, property_array, N)

    IMPLICIT NONE

    !----- INPUT PARAMETERS -----

    ! Number of x values
    INTEGER                  :: N

    ! x values
    REAL(kind = 8)           :: x(N)

    ! value at x = 0 (y_0) and infinite depth (y_inf)
    REAL(kind = 8)           :: y_0, y_inf

    ! attenuation factor
    REAL(kind = 8)           :: x_att

    !----- VARIABLES TO CHANGE ------
    REAL(kind = 8)           :: property_array(N)

    !--------------------------------------------------------------------------

    property_array = y_inf + (y_0 - y_inf) * exp(- x / x_att)

END SUBROUTINE pexp


SUBROUTINE psig (x, y_0, y_inf, x_L, x_att, property_array, N)

    IMPLICIT NONE

    !----- INPUT PARAMETERS -----

    ! Number of x values
    INTEGER                  :: N

    ! x values
    REAL(kind = 8)           :: x(N)

    ! value at x = 0 (y_0) and infinite depth (y_inf)
    REAL(kind = 8)           :: y_0, y_inf

    ! x transition
    REAL(kind = 8)           :: x_L

    ! attenuation factor
    REAL(kind = 8)           :: x_att

    !----- VARIABLES TO CHANGE ------
    REAL(kind = 8)           :: property_array(N)

    !--------------------------------------------------------------------------

    property_array = y_inf + (y_0 - y_inf) * exp(-(x - x_L)/(0.25 * x_att))/(1 + exp(-(x - x_L)/(0.25 * x_att)))

END SUBROUTINE psig


SUBROUTINE setup_v_por_int (rsed, por_0, por_inf, por_int, property_array, N)

    IMPLICIT NONE

    !----- INPUT PARAMETERS -----

    ! Number of v values (= N_grid + 1)
    INTEGER                  :: N

    ! Sedimentation rate
    REAL(kind = 8)           :: rsed

    ! porosity at x = 0 (por_0) and infinite depth (por_inf)
    REAL(kind = 8)           :: por_0, por_inf

    ! porosity at interfaces
    REAL(kind = 8)           :: por_int(N)

    !----- VARIABLES TO CHANGE ------
    REAL(kind = 8)           :: property_array(N)

    !--------------------------------------------------------------------------

    property_array = ((rsed * (1 - por_0)) / (1 - por_inf) * por_inf) / por_int

END SUBROUTINE setup_v_por_int


SUBROUTINE setup_v_svf_int (rsed, por_0, por_int, property_array, N)

    IMPLICIT NONE

    !----- INPUT PARAMETERS -----

    ! Number of v values (= N_grid + 1)
    INTEGER                  :: N

    ! Sedimentation rate
    REAL(kind = 8)           :: rsed

    ! porosity at x = 0 (por_0)
    REAL(kind = 8)           :: por_0

    ! porosity at interfaces
    REAL(kind = 8)           :: por_int(N)

    !----- VARIABLES TO CHANGE ------
    REAL(kind = 8)           :: property_array(N)

    !--------------------------------------------------------------------------

    property_array = (rsed * (1 - por_0)) / (1 - por_int)

END SUBROUTINE setup_v_svf_int

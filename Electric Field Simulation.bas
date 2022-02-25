REM Charged Particle Simulation
REM Maxwell Goldberg 2/12/22

DEFDBL A-Z 'careful, this resets all variables, keep at beginning of program

SCREEN 12
s = 5
WINDOW (-s - 1, -s)-(s - 1, s)

N = 2 'Number of charges
DIM SHARED q(N)
DIM SHARED x(N)
DIM SHARED y(N)
k = 1

q(1) = -1
q(2) = 1

x(1) = -1
y(1) = 0

x(2) = 1
y(2) = 0

q = 1
m = 1
x = 0
y = 2
vx = 0
vy = 0

t = 0
dt = 0.00001

FOR i = 1 TO 2
    CIRCLE (x(i), y(i)), 0.5
NEXT i
PSET (x, y)

SLEEP

q$ = "w"
WHILE t < 1001 AND q$ <> "q"

    Fx = 0
    Fy = 0

    FOR i = 1 TO 2
        r = SQR((x - x(i)) ^ 2 + (y - y(i)) ^ 2)
        Fx = Fx + k * q * q(i) * (x - x(i)) / r ^ 3
        Fy = Fy + k * q * q(i) * (y - y(i)) / r ^ 3
    NEXT i

    ax = Fx / m
    vx = vx + ax * dt
    x = x + vx * dt

    ay = Fy / m
    vy = vy + ay * dt
    y = y + vy * dt

    PSET (x, y), _ROUND(t / 10) MOD 15 + 1

    'KE = 0.5 * m * v ^ 2
    'PE = 0
    'FOR i = 1 TO 2
    '    PE = PE + -k * q * q(i) / SQR((x - x(i)) ^ 2 + (y - y(i)) ^ 2)
    'NEXT i
    'PSET (t, PE + KE)
    t = t + dt
    'PRINT t
    q$ = INKEY$
WEND
SLEEP
CLS
FOR i = 1 TO 2
    CIRCLE (x(i), y(i)), 0.25
NEXT i
SLEEP

dt = 0.001
d = 0.04
inc = 2 * d
l = s + 1 - d
FOR a = -l TO l STEP inc
    FOR b = -l TO l STEP inc
        x0 = a
        y0 = b
        q0 = q
        m0 = m
        dt0 = dt
        c = ComputePath(x0, y0, q0, m0, dt0, 8001)

        IF c <> 0 THEN
            LINE (a - d, b - d)-(a + d, b + d), c, BF
        END IF
    NEXT b
NEXT a

CIRCLE (x(1), y(1)), 0.25
CIRCLE (x(2), y(2)), 0.25

SLEEP
END




FUNCTION ComputePath (x, y, q, m, dt, L)
    k = 1
    FOR j = 1 TO L
        Fx = 0
        Fy = 0

        FOR i = 1 TO 2
            r = SQR((x - x(i)) ^ 2 + (y - y(i)) ^ 2)
            Fx = Fx + k * q * q(i) * (x - x(i)) / r ^ 3
            Fy = Fy + k * q * q(i) * (y - y(i)) / r ^ 3
        NEXT i

        ax = Fx / m
        vx = vx + ax * dt
        x = x + vx * dt

        ay = Fy / m
        vy = vy + ay * dt
        y = y + vy * dt

        dist = SQR((x - x(1)) ^ 2 + (y - y(1)) ^ 2)

        IF dist > 5 THEN 'dist relative to left charge!
            'PRINT j
            'PRINT dist
            'SLEEP
            jj = j \ 500
            ComputePath = ABS(jj * (14 >= jj) + 14 * (14 < jj))
            EXIT FUNCTION
        END IF
    NEXT j
    ComputePath = 15
END FUNCTION

import numpy as np

# Multi-objective optimization engine
# pip install pymoo
from pymoo.algorithms.moo.nsga3 import NSGA3
from pymoo.core.problem import Problem
from pymoo.optimize import minimize
from pymoo.operators.sampling.rnd import BinaryRandomSampling
from pymoo.operators.crossover.pntx import TwoPointCrossover
from pymoo.operators.mutation.bitflip import BitflipMutation
from pymoo.util.ref_dirs import get_reference_directions


# ============================================================
# Define metric values for overall system
# ============================================================

OPERATING_COST = np.array(
    [
        [180, 240, 420],  # D1
        [180, 170, 360],  # D2
        [150, 165, 200],  # D3
        [160, 185, np.nan],  # D4
        [140, 175, 230],  # D5
        [160, 210, np.nan],  # D6
        [155, 170, np.nan],  # D7
        [195, 160, np.nan],  # D8
    ],
    dtype=float,
)

PROPULSION_RELIABILITY = np.array(
    [
        [99.6, 99.3, 97.8],
        [99.5, 99.7, 99.9],
        [98.8, 99.5, 99.3],
        [99.1, 99.4, np.nan],
        [98.5, 99.5, 99.95],
        [99.2, 99.1, np.nan],
        [99.1, 99.2, np.nan],
        [99.8, 98.9, np.nan],
    ],
    dtype=float,
) / 100.0

SYSTEM_DURABILITY = np.array(
    [
        [9.0, 8.0, 7.0],
        [7.5, 8.5, 9.0],
        [8.0, 9.0, 8.5],
        [8.5, 9.5, np.nan],
        [7.0, 8.5, 10.0],
        [8.0, 9.0, np.nan],
        [8.0, 8.5, np.nan],
        [9.0, 8.0, np.nan],
    ],
    dtype=float,
)

CERTIFICATION_SCHEDULE = np.array(
    [
        [2.0, 3.0, 6.0],
        [1.5, 1.5, 5.0],
        [1.5, 2.5, 4.0],
        [2.0, 2.5, np.nan],
        [1.5, 3.0, 4.5],
        [2.0, 3.5, np.nan],
        [2.0, 3.0, np.nan],
        [3.5, 2.0, np.nan],
    ],
    dtype=float,
)


def decimal2binary(x: int, length: int) -> np.ndarray:
    bits = [int(c) for c in format(int(x), f"0{length}b")]
    return np.array(bits[-length:], dtype=int)


def binary2decimal(bits: np.ndarray) -> int:
    bits = np.asarray(bits, dtype=int).flatten()
    val = 0
    for bit in bits:
        val = (val << 1) | int(bit)
    return int(val)


def int2bit(xhat: np.ndarray) -> np.ndarray:
    xhat = np.asarray(xhat, dtype=int).flatten()
    chunks = [
        decimal2binary(xhat[0], 2),
        decimal2binary(xhat[1], 2),
        decimal2binary(xhat[2], 2),
        decimal2binary(xhat[3], 1),
        decimal2binary(xhat[4], 1),
        decimal2binary(xhat[5], 1),
        decimal2binary(xhat[6], 1),
        decimal2binary(xhat[7], 1),
        decimal2binary(xhat[8], 1),
        decimal2binary(xhat[9], 2),
        decimal2binary(xhat[10], 1),
        decimal2binary(xhat[11], 1),
        decimal2binary(xhat[12], 1),
        decimal2binary(xhat[13], 2),
        decimal2binary(xhat[14], 2),
        decimal2binary(xhat[15], 2),
        decimal2binary(xhat[16], 2),
        decimal2binary(xhat[17], 1),
    ]
    return np.concatenate(chunks).astype(int)


def bit2int(x: np.ndarray) -> np.ndarray:
    x = np.asarray(x, dtype=int).flatten()
    return np.array(
        [
            binary2decimal(x[0:2]),
            binary2decimal(x[2:4]),
            binary2decimal(x[4:6]),
            binary2decimal(x[6:7]),
            binary2decimal(x[7:8]),
            binary2decimal(x[8:9]),
            binary2decimal(x[9:10]),
            binary2decimal(x[10:11]),
            binary2decimal(x[11:12]),
            binary2decimal(x[12:14]),
            binary2decimal(x[14:15]),
            binary2decimal(x[15:16]),
            binary2decimal(x[16:17]),
            binary2decimal(x[17:19]),
            binary2decimal(x[19:21]),
            binary2decimal(x[21:23]),
            binary2decimal(x[23:25]),
            binary2decimal(x[25:26]),
        ],
        dtype=int,
    )


def cost(xhat: np.ndarray) -> float:
    c = (
        OPERATING_COST[0, xhat[0]]
        + OPERATING_COST[1, xhat[1]]
        + OPERATING_COST[2, xhat[2]]
    )
    d4 = xhat[3:9]
    wing_cost = np.sum(d4[0:4] * OPERATING_COST[3, 0])
    nose_tail_cost = np.sum(d4[4:6] * OPERATING_COST[3, 1])
    c += wing_cost + nose_tail_cost
    c += OPERATING_COST[4, xhat[9]] + OPERATING_COST[5, xhat[10]]
    d7 = xhat[11:17]
    d7_val = 1 if np.any(d7 == 2) else 0
    c += OPERATING_COST[6, d7_val]
    c += OPERATING_COST[7, xhat[17]]
    return float(c)


def certification(xhat: np.ndarray) -> float:
    seq_time = (
        CERTIFICATION_SCHEDULE[0, xhat[0]]
        + CERTIFICATION_SCHEDULE[4, xhat[9]]
        + CERTIFICATION_SCHEDULE[5, xhat[10]]
    )
    d2_time = CERTIFICATION_SCHEDULE[1, xhat[1]]
    d3_time = CERTIFICATION_SCHEDULE[2, xhat[2]]
    d4 = xhat[3:9]
    d4_time = np.max(d4 * CERTIFICATION_SCHEDULE[3, 1])
    d7 = xhat[11:17]
    num_partitions = len(np.unique(d7))
    d7_time = CERTIFICATION_SCHEDULE[6, 1] if num_partitions == 3 else CERTIFICATION_SCHEDULE[6, 0]
    d8_time = CERTIFICATION_SCHEDULE[7, xhat[17]]
    par_time = max(d2_time, d3_time, d4_time, d7_time, d8_time)
    return float(max(seq_time, par_time))


def reliability(xhat: np.ndarray) -> float:
    r_d1 = PROPULSION_RELIABILITY[0, xhat[0]]
    r_d3 = PROPULSION_RELIABILITY[2, xhat[2]]
    d4 = xhat[3:9]
    wing = d4[0:4]
    nose = d4[4:6]
    wing_rel = np.prod(1 - wing * (1 - PROPULSION_RELIABILITY[3, 0]))
    nose_rel = np.prod(1 - nose * (1 - PROPULSION_RELIABILITY[3, 1]))
    r_d4 = 1 - (1 - wing_rel) * (1 - nose_rel)
    r_series = r_d1 * r_d3 * r_d4
    r_d5 = PROPULSION_RELIABILITY[4, xhat[9]]
    r_d6 = PROPULSION_RELIABILITY[5, xhat[10]]
    d7 = xhat[11:17]
    num_partitions = len(np.unique(d7))
    r_d7 = PROPULSION_RELIABILITY[6, 1] if num_partitions == 3 else PROPULSION_RELIABILITY[6, 0]
    r_parallel = 1 - (1 - r_d5) * (1 - r_d6) * (1 - r_d7)
    return float(r_series * r_parallel)


def durability(xhat: np.ndarray) -> float:
    series_vals = [
        SYSTEM_DURABILITY[0, xhat[0]],
        SYSTEM_DURABILITY[1, xhat[1]],
        SYSTEM_DURABILITY[2, xhat[2]],
    ]
    d4 = xhat[3:9]
    d4_time = np.max(d4 * SYSTEM_DURABILITY[3, 1])
    series_vals.append(d4_time)
    series_min = min(series_vals)
    parallel_vals = [SYSTEM_DURABILITY[4, xhat[9]], SYSTEM_DURABILITY[5, xhat[10]]]
    d7 = xhat[11:17]
    num_partitions = len(np.unique(d7))
    d7_time = SYSTEM_DURABILITY[6, 1] if num_partitions == 3 else SYSTEM_DURABILITY[6, 0]
    parallel_vals.append(d7_time)
    return float(min(series_min, max(parallel_vals)))


def evaluate(xhat: np.ndarray) -> np.ndarray:
    return np.array(
        [
            cost(xhat),
            certification(xhat),
            reliability(xhat),
            durability(xhat),
        ],
        dtype=float,
    )


def repair_d4(d1: int, d3: int, d4_1: int, d4_2: int, d4_3: int, d4_4: int, d4_5: int, d4_6: int) -> np.ndarray:
    while True:
        if d1 in (0, 1):
            d4_5 = 0
        if d1 == 2:
            d4_5 = 0
            d4_6 = 0

        required_count = [1, 2, 4][d3]
        valid_positions = [1, 2, 3, 4, 5, 6]
        if d1 in (0, 1):
            valid_positions = [p for p in valid_positions if p != 5]
        if d1 == 2:
            valid_positions = [p for p in valid_positions if p not in (5, 6)]

        d4_1 = d4_2 = d4_3 = d4_4 = d4_5 = d4_6 = 0
        chosen = np.random.choice(valid_positions, size=required_count, replace=False)

        d4_1 = 1 if 1 in chosen else 0
        d4_2 = 1 if 2 in chosen else 0
        d4_3 = 1 if 3 in chosen else 0
        d4_4 = 1 if 4 in chosen else 0
        d4_5 = 1 if 5 in chosen else 0
        d4_6 = 1 if 6 in chosen else 0

        if (d4_1 + d4_2 + d4_3 + d4_4 + d4_5 + d4_6) == required_count:
            return np.array([d4_1, d4_2, d4_3, d4_4, d4_5, d4_6], dtype=int)


def repair_d7(d5: int, d7: np.ndarray) -> np.ndarray:
    d7 = np.array(d7, dtype=int).copy()
    d7[0] = 0

    if d5 in (1, 2):
        d7 = np.clip(d7, 0, 2)
        groups = np.unique(d7)
        missing = [g for g in (0, 1, 2) if g not in groups]
        if len(missing) > 0:
            idx = np.random.choice([1, 2, 3, 4, 5], size=len(missing), replace=False)
            for i, g in zip(idx, missing):
                d7[i] = g

    for g in (0, 1, 2):
        if g not in d7:
            j = np.random.choice([1, 2, 3, 4, 5], size=1)[0]
            d7[j] = g
    return d7


def repair_bits(x: np.ndarray) -> np.ndarray:
    xhat = bit2int(x)
    d1, d2, d3 = xhat[0], xhat[1], xhat[2]
    d4_1, d4_2, d4_3, d4_4, d4_5, d4_6 = xhat[3:9]
    d5, d6 = xhat[9], xhat[10]
    d7_1, d7_2, d7_3, d7_4, d7_5, d7_6 = xhat[11:17]
    d8 = xhat[17]

    if d1 == 3:
        d1 = np.random.choice([0, 1, 2])
    if d2 == 3:
        d2 = np.random.choice([0, 1, 2])
    if d3 == 3:
        d3 = np.random.choice([0, 1, 2])
    if d4_5 == 1:
        d4_5 = 0
    if d5 == 3:
        d5 = np.random.choice([0, 1, 2])
    if d7_1 == 1:
        d7_1 = 0
    if d7_3 == 3:
        d7_3 = np.random.choice([0, 1, 2])
    if d7_4 == 3:
        d7_4 = np.random.choice([0, 1, 2])
    if d7_5 == 3:
        d7_5 = np.random.choice([0, 1, 2])
    if d7_6 == 3:
        d7_6 = np.random.choice([0, 1, 2])

    if d1 == 2 and d4_6 == 1:
        d4 = repair_d4(d1, d3, d4_1, d4_2, d4_3, d4_4, d4_5, d4_6)
        d4_1, d4_2, d4_3, d4_4, d4_5, d4_6 = d4

    if (d4_1 + d4_2 + d4_3 + d4_4 + d4_5 + d4_6) != [1, 2, 4][d3]:
        d4 = repair_d4(d1, d3, d4_1, d4_2, d4_3, d4_4, d4_5, d4_6)
        d4_1, d4_2, d4_3, d4_4, d4_5, d4_6 = d4

    if d5 in (1, 2):
        d7 = np.array([d7_1, d7_2, d7_3, d7_4, d7_5, d7_6], dtype=int)
        if (
            d7_1 != 0
            or 0 not in d7
            or 1 not in d7
            or 2 not in d7
        ):
            d7 = repair_d7(d5, d7)
            d7_1, d7_2, d7_3, d7_4, d7_5, d7_6 = d7

    repaired_xhat = np.array(
        [d1, d2, d3, d4_1, d4_2, d4_3, d4_4, d4_5, d4_6, d5, d6, d7_1, d7_2, d7_3, d7_4, d7_5, d7_6, d8],
        dtype=int,
    )
    bits = int2bit(repaired_xhat)
    if len(bits) < 26:
        bits = np.concatenate([bits, np.zeros(26 - len(bits), dtype=int)])
    elif len(bits) > 26:
        bits = bits[:26]
    return bits.astype(int)


class EVTOLProblem(Problem):
    def __init__(self):
        # pymoo minimizes by default.
        # Here we minimize cost/certification and maximize reliability/durability
        # by negating the last two objectives.
        super().__init__(n_var=26, n_obj=4, n_ieq_constr=0, xl=0, xu=1, vtype=int)

    def _evaluate(self, X, out, *args, **kwargs):
        F = np.zeros((X.shape[0], 4), dtype=float)
        for i in range(X.shape[0]):
            bits = np.asarray(X[i], dtype=int)
            bits = repair_bits(bits)
            xhat = bit2int(bits)
            m1, m2, m3, m4 = evaluate(xhat)
            F[i, 0] = m1
            F[i, 1] = m2
            F[i, 2] = -m3
            F[i, 3] = -m4
        out["F"] = F


def main():
    ref_dirs = get_reference_directions("das-dennis", 4, n_partitions=10)

    algorithm = NSGA3(
        pop_size=50,
        ref_dirs=ref_dirs,
        sampling=BinaryRandomSampling(),
        crossover=TwoPointCrossover(prob=0.8),
        mutation=BitflipMutation(prob=1.0 / 26.0),
    )

    problem = EVTOLProblem()

    res = minimize(
        problem,
        algorithm,
        ("n_gen", 100),
        seed=1,
        verbose=True,
    )

    print("\nFinal Pareto set size:", 0 if res.F is None else len(res.F))
    if res.X is not None and len(res.X) > 0:
        first_bits = np.asarray(res.X[0], dtype=int)
        first_xhat = bit2int(repair_bits(first_bits))
        print("Example solution (decoded xhat):", first_xhat.tolist())
        print("Example metrics [cost, cert, rel, durability]:", evaluate(first_xhat).tolist())


if __name__ == "__main__":
    main()

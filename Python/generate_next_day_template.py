import os

dir_path: str = os.path.dirname(os.path.realpath(__file__))

num_lst: list[str] = [x[-3:] for x in os.listdir(dir_path) if "problem" in x]
num: int = int(sorted(num_lst)[-1]) + 1

str_num: str = '{:>03d}'.format(num)

os.system(fr"mkdir {dir_path}/problem{str_num}")

# check if in python dir else change dir to python
os.chdir(dir_path)
os.system(fr"produce make_problem{str_num}")

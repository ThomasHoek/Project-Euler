from distutils.dir_util import copy_tree
import os

dir_path: str = os.path.dirname(os.path.realpath(__file__))
template_path: str = dir_path + r"\template"

num: int = int([x for x in os.listdir(dir_path)
               if "problem" in x][-1][-3:]) + 1
str_num: str = '{:>03d}'.format(num)
new_path: str = dir_path + fr"\problem{str_num}"

copy_tree(f"{template_path}", f"{new_path}")

os.rename(rf"{new_path}/problemx.py", rf"{new_path}/problem{num}.py")
print(f"made day {num}")

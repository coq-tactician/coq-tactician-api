eval "$(conda shell.bash hook)"
conda deactivate
conda deactivate
module use ~/.local/easybuild/modules/all
module load git OCaml util-linux GCC
conda activate tactician

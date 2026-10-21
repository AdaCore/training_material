# Génère les slides PDF via Pandoc
build source="./courses/rust_essentials/520_memory_and_runtime_constraints.rst" output="slides_520.pdf":
    python3 ./pandoc/pandoc_fe.py \
        --source {{source}} \
        --output {{output}} \
        --extension pdf \
        --directories ./images,./support_files \
        --theme adacore \
        --color adacore \
        --filter beamer_filter.py

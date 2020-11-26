pandoc $1 \
    -o $2 \
    --resource-path=images/: \
    --data-dir=support_files \
    --template=adacore

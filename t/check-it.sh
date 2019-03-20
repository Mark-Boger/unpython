#!/usr/bin/bash

# This is probably bad but w.e

L_FILE=/tmp/unpython-lisp-check
P_FILE=/tmp/unpython-python-check
LOG=output.diff

function run_python {
    python <<EOF
import pickle

with open("data/large_dict.pkl", "rb") as f:
     large_dict = pickle.load(f)

with open("${P_FILE}", "w") as f:
     for k in large_dict.keys():
         f.write(k)
         f.write('\n')
EOF
}

function run_lisp {

    sbcl --noinform --disable-ldb --lose-on-corruption --disable-debugger --noprint <<EOF
(asdf:load-system :unpython)

(let ((table (unpython:load-pickle "data/large_dict.pkl")))
     (with-open-file (out "${L_FILE}" :if-does-not-exist :create
                                      :if-exists :supersede
                                      :direction :output)
     (loop for k being the hash-keys in table
              do (write-line k out))))
EOF

}

function sort_it {
    sort ${1} > /tmp/unpython-sorting && mv /tmp/unpython-sorting ${1}
}

run_python
run_lisp

sort_it ${L_FILE} 
sort_it ${P_FILE}

size_l=$(cat ${L_FILE} | wc -l)
size_p=$(cat ${P_FILE} | wc -l)

report=$(diff -u --color=always ${L_FILE} ${P_FILE})

res=$?

if [ ${size_l} -eq ${size_p} ]; then
    echo "Outputs look to be the same size: ${size_l}"

    if [[ ${res} -ne 0 ]]; then
        echo -n "Outputs aren't the same!"
        echo " Please see ${LOG} for more info."
        echo "${report}" > ${LOG}

        exit 1
        rm ${L_FILE} ${P_FILE}
    fi

    echo "Outputs look to have the same outputs!"
else
    echo "Outputs aren't the same! L ${size_l} != ${size_p} P"
    echo "Please see ${LOG} for more info"
    echo "${report}" > ${LOG}

    exit 1
    rm ${L_FILE} ${P_FILE}
fi

rm ${L_FILE} ${P_FILE}

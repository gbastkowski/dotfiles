#!/usr/bin/env sh

upgrade_system_and_packages() {
    softwareupdate -l
    brew update && brew upgrade
}

upgrade_python_packages() {
    echo "updating pip stuff"
    for i in $(pip list -o | awk 'NR > 2 {print $1}')
    do
        pip install -U $i
    done
}

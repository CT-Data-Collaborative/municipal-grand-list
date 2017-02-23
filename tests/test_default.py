import pytest

##################################################################
#
# Basic testing script for Municipal-Grand-List
# Created by Jenna Daly
# On 02/15/2017
#
##################################################################


def test_towns(towns, geographies):
    assert set([x['Town'] for x in towns]) == set(geographies)


def test_dataset_row_counts(rowcount):
    assert rowcount.actual == rowcount.expected

def test_spotcheck_testing(spotcheck_results):
    for check in spotcheck_results:
        assert check.expected == check.actual

def test_geography_count(geographies, geography_units_count):
    assert len(geographies) == geography_units_count
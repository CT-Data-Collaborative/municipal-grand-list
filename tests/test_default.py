import pytest

##################################################################
#
# Basic testing script for Municipal-Grand-List
# Created by Jenna Daly
# On 02/15/2017
#
##################################################################


def test_dataset_row_counts(rowcount):
    assert rowcount.actual == rowcount.expected

def test_spotcheck_testing(spotcheck_results):
    for check in spotcheck_results:
        assert check.expected == check.actual

def test_geography_count(geographies, geography_units_count):
    assert len(geographies) == geography_units_count

def test_schema_validation(schema_test):
    assert schema_test

def test_schema_validation(schema):
    dimensions = [s for s in schema if s['dimension']]
    for d in dimensions:
        assert isinstance(d["constraints"]["enum"], list)

def test_source_validation(source_options, source):
    for s in source:
        assert s['name'] in source_options

def test_domain_subdomain_validation(domain_map, domain, subdomain):
    assert domain in domain_map
    assert subdomain in domain_map[domain]

def test_towns(towns, geographies):
    assert set([x['Town'] for x in towns if x['Town'] != 'Connecticut']) == set(geographies)



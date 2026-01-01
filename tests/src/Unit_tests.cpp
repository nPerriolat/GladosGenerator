/*
** EPITECH PROJECT, 2024
** 209poll
** File description:
** Unit tests
*/

#include <criterion/criterion.h>
#include "Poll.hpp"

using namespace poll;

Test(Computing, test_subject_1)
{
    int argc = 4;
    char *argv[] = {
        "program_name",
        "10000",
        "500",
        "42.24",
        NULL
    };
    Data data(argc, argv);
    std::string result;
    std::stringstream stream;
    stream << "Population size:\t\t10000" << std::endl
            << "Sample size:\t\t\t500" << std::endl
            << "Voting intentions:\t\t42.24%" << std::endl
            << "Variance:\t\t\t0.000464" << std::endl
            << "95% confidence interval:\t[38.02%; 46.46%]" << std::endl
            << "99% confidence interval:\t[36.68%; 47.80%]" << std::endl;
    std::string expected = stream.str();

    try {
        data.compute();
        result = data.display();
    } catch (std::exception &e) {
        cr_assert(false, "Computing : expected no error");
    }
    cr_assert_eq(result, expected, "Initialisation : expected matching ouput");
}

Test(Computing, test_subject_2)
{
    int argc = 4;
    char *argv[] = {
        "program_name",
        "10000",
        "100",
        "45",
        NULL
    };
    Data data(argc, argv);
    std::string result;
    std::stringstream stream;
    stream << "Population size:\t\t10000" << std::endl
            << "Sample size:\t\t\t100" << std::endl
            << "Voting intentions:\t\t45.00%" << std::endl
            << "Variance:\t\t\t0.002450" << std::endl
            << "95% confidence interval:\t[35.30%; 54.70%]" << std::endl
            << "99% confidence interval:\t[32.23%; 57.77%]" << std::endl;
    std::string expected = stream.str();

    try {
        data.compute();
        result = data.display();
    } catch (std::exception &e) {
        cr_assert(false, "Computing : expected no error");
    }
    cr_assert_eq(result, expected, "Initialisation : expected matching ouput");
}

use strict;
use warnings;
use threads;
use threads::shared;
use Time::HiRes qw(gettimeofday tv_interval);

my $NUM_CPU = `nproc`;
chomp $NUM_CPU;

sub count_numbers {
    my ($start, $end) = @_;
    my $count = 0;
    for (my $i = $start; $i <= $end; $i++) {
        $count++;
    }
    return $count;
}

sub spawn_counter {
    my ($start, $end) = @_;
    return threads->create(sub {
        my $count = count_numbers($start, $end);
        return $count;
    });
}

sub collect_results {
    my ($threads) = @_;
    my @results;
    foreach my $thread (@$threads) {
        my $count = $thread->join();
        push @results, $count;
    }
    return @results;
}

sub main {
    my $per_chunk = int(1000000000 / $NUM_CPU);
    my $remainder = 1000000000 % $NUM_CPU;

    my $start_time = [gettimeofday];

    my @threads;
    for (my $i = 0; $i < $NUM_CPU; $i++) {
        my $start = $i * $per_chunk + 1;
        my $end = $start + $per_chunk - 1;
        $end += $remainder if ($i == $NUM_CPU - 1);
        push @threads, spawn_counter($start, $end);
    }

    my @results = collect_results(\@threads);
    my $total_count = 0;
    foreach my $count (@results) {
        $total_count += $count;
    }

    my $end_time = [gettimeofday];
    my $execution_time = tv_interval($start_time, $end_time) * 1000;

    print "Count: $total_count\n";
    print "Execution Time: $execution_time milliseconds\n";
}

main();

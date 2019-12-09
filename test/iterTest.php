<?php

namespace iter;

use PHPUnit\Framework\TestCase;

class IterTest extends TestCase {
    /** @dataProvider provideTestRange */
    public function testRange($start, $end, $step, $resultArray): void
    {
        $this->assertSame($resultArray, toArray(range($start, $end, $step)));
    }

    public function provideTestRange(): array
    {
        return [
            [0, 10, null,  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]],
            [0, 10, 2,  [0, 2, 4, 6, 8, 10]],
            [0, 3, 0.5, [0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0]],
            [10, 0, null, [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]],
            [10, 0, -2, [10, 8, 6, 4, 2, 0]],
            [3, 0, -0.5, [3, 2.5, 2.0, 1.5, 1.0, 0.5, 0.0]],
            [5, 5, 0, [5]]
        ];
    }

    
    public function testRangeStepMustBePositive(): void
    {
        $this->expectException(\InvalidArgumentException::class);
        $this->expectExceptionMessage('If start < end the step must be positive');

        toArray(range(0, 10, -1));
    }

    
    public function testRangeStepMustBeNegative(): void
    {
        $this->expectException(\InvalidArgumentException::class);
        $this->expectExceptionMessage('If start > end the step must be negative');

        toArray(range(10, 0, 1));
    }

    public function testMapKeys(): void
    {
        $range = range(0, 5);
        $mapped = mapKeys($range, static function($n) { return $n * 3; });
        $this->assertSame(
            [0 => 0, 3 => 1, 6 => 2, 9 => 3, 12 => 4, 15 => 5],
            toArrayWithKeys($mapped)
        );

        $mapped = mapKeys(['A' => 1, 'B' => 2, 'C' => 3], 'strtolower');
        $this->assertSame(
            ['a' => 1, 'b' => 2, 'c' => 3],
            toArrayWithKeys($mapped)
        );
    }

    public function testFlatMap(): void
    {
        $this->assertSame(
            [-1, 1, -2, 2, -3, 3, -4, 4, -5, 5],
            toArray(flatMap([1, 2, 3, 4, 5], static function($v) {
                return [-$v, $v];
            }))
        );
        $this->assertSame(
            [],
            toArray(flatMap([1, 2, 3, 4, 5], static function() { return []; }))
        );
    }

    public function testReindex(): void
    {
        $iter = reindex(['a', 'b', 'c', 'd', 'e'], 'strtoupper');
        $this->assertSame(
            ['A' => 'a', 'B' => 'b', 'C' => 'c', 'D' => 'd', 'E' => 'e'],
            toArrayWithKeys($iter)
        );

        $iter = reindex([1, 2, 3, 4], func\operator('*', 2));
        $this->assertSame(
            [2 => 1, 4 => 2, 6 => 3, 8 => 4],
            toArrayWithKeys($iter)
        );
    }

    public function testApply(): void
    {
        $range = range(0, 5);
        $result = [];
        apply($range, static function($n) use (&$result) { $result[] = $n; });

        $this->assertSame([0, 1, 2, 3, 4, 5], $result);
    }

    public function testEnumerateIsAliasOfToPairs(): void
    {
        $this->assertSame(toArray(toPairs(['a', 'b'])), toArray(enumerate(['a', 'b'])));
    }

    public function testToPairs(): void
    {
         $this->assertSame([[0, 'a'], [1, 'b']], toArray(toPairs(['a', 'b'])));
    }

    public function testToPairsWithStringKeys(): void
    {
        $enumerated = toPairs([
            'a' => 1,
            'b' => 2,
        ]);
        $this->assertSame([['a', 1], ['b', 2]], toArray($enumerated));
    }

    public function testFromPairs(): void
    {
        $this->assertSame(['a', 'b'], toArrayWithKeys(fromPairs([[0, 'a'], [1, 'b']])));
    }

    public function testFromPairsInverseToPairs(): void
    {
        $map = ['a' => 1, 'b' => 2];
        $this->assertSame($map, toArrayWithKeys(fromPairs(toPairs($map))));
    }

    public function testZip(): void
    {
        $zipped = zip(range(0, 5), range(5, 0, -1));
        $this->assertSame([[0,5], [1,4], [2,3], [3,2], [4,1], [5,0]], toArray($zipped));
    }

    public function testZipEmpty(): void
    {
        $res = toArray(zip());
        $this->assertSame([], $res);
    }

    public function testZipKeyValue(): void
    {
        $zipped = zipKeyValue(range(5, 0, -1), range(0, 5));
        $this->assertSame([5=>0, 4=>1, 3=>2, 2=>3, 1=>4, 0=>5], toArrayWithKeys($zipped));
    }

    public function testChain(): void
    {
        $chained = chain(range(1, 3), range(4, 6), range(7, 9));
        $this->assertSame([1, 2, 3, 4, 5, 6, 7, 8, 9], toArray($chained));

        // empty chain
        $this->assertSame([], toArray(chain()));
    }

    public function testSlice(): void
    {
        $this->assertSame(
            [5, 6, 7, 8, 9],
            toArray(slice(range(0, INF), 5, 5))
        );
        $this->assertSame(
            [5, 6, 7, 8, 9],
            toArray(slice(range(0, 9), 5))
        );

        // empty slice
        $this->assertSame([], toArray(slice(range(0, INF), 0, 0)));
    }

    public function testSliceDoNotTakeElementsAboveEndIndex(): void
    {
        $takenElements = 0;
        $iterator = static function () use (&$takenElements) {
            foreach (range(0, INF) as $item) {
                $takenElements++;
                yield $item;
            }
        };

        $this->assertSame(
            [0, 1, 2],
            toArray(slice($iterator(), 0, 3))
        );

        $this->assertSame(3, $takenElements);
    }

    public function testSliceNegativeLengthError(): void
    {
        $this->expectException(\InvalidArgumentException::class);
        $this->expectExceptionMessage('Length must be non-negative');

        toArray(slice(range(0, INF), 0, -1));
    }

    
    public function testSliceNegativeStartOffsetError(): void
    {
        $this->expectException(\InvalidArgumentException::class);
        $this->expectExceptionMessage('Start offset must be non-negative');

        toArray(slice(range(0, INF), -1, 5));
    }

    public function testSliceNegativeStartOffsetErrorWithZeroLength(): void
    {
        $this->expectException(\InvalidArgumentException::class);
        $this->expectExceptionMessage('Start offset must be non-negative');

        toArray(slice(range(0, INF), -1, 0));
    }

    public function testTakeDrop(): void
    {
        $this->assertSame([1, 2, 3], toArray(take(3, [1, 2, 3, 4, 5])));
        $this->assertSame([4, 5], toArray(drop(3, [1, 2, 3, 4, 5])));
        $this->assertSame([], toArray(take(3, [])));
        $this->assertSame([], toArray(drop(3, [])));
    }

    public function testRepeat(): void
    {
        $this->assertSame([1, 1, 1, 1, 1], toArray(repeat(1, 5)));
        $this->assertSame([], toArray(repeat(1, 0)));
    }

    
    public function testRepeatNegativeNumError(): void
    {
        $this->expectException(\InvalidArgumentException::class);
        $this->expectExceptionMessage('Number of repetitions must be non-negative');

        toArray(repeat(1, -1));
    }

    public function testKeyValue(): void
    {
        $array = ['a' => 'b', 'c' => 'd', 'e' => 'f'];
        $this->assertSame(['b', 'd', 'f'], toArrayWithKeys(values($array)));
        $this->assertSame(['a', 'c', 'e'], toArrayWithKeys(keys($array)));
    }

    public function testReduce(): void
    {
        $this->assertSame(15, reduce(range(1, 5), func\operator('+'), 0));
        $this->assertSame(120, reduce(range(1, 5), func\operator('*'), 1));
    }

    public function testComplexReduce(): void
    {
        $this->assertSame('abcdef', reduce(['a' => 'b', 'c' => 'd', 'e' => 'f'], static function ($acc, $value, $key) {
            return $acc . $key . $value;
        }, ''));
    }

    public function testReductions(): void
    {
        $this->assertSame(
            [1, 3, 6, 10, 15],
            toArrayWithKeys(reductions(range(1, 5), func\operator('+'), 0))
        );
        $this->assertSame(
            [1, 2, 6, 24, 120],
            toArrayWithKeys(reductions(range(1, 5), func\operator('*'), 1))
        );
    }

    public function testComplexReductions(): void
    {
        $this->assertSame(
            ['ab', 'abcd', 'abcdef'],
            toArrayWithKeys(reductions(['a' => 'b', 'c' => 'd', 'e' => 'f'], static function ($acc, $value, $key) {
                return $acc . $key . $value;
            }, ''))
        );
    }

    public function testAnyAll(): void
    {
        $this->assertTrue(all(range(1, 10), func\operator('>', 0)));
        $this->assertFalse(all(range(-5, 5), func\operator('>', 0)));
        $this->assertTrue(any(range(-5, 5), func\operator('>', 0)));
        $this->assertFalse(any(range(-10, 0), func\operator('>', 0)));
    }

    public function testSearch(): void
    {
        $iter = new \ArrayIterator(['foo', 'bar', 'baz']);
        $this->assertSame('baz', search($iter, func\operator('===', 'baz')));

        $iter = new \ArrayIterator(['foo', 'bar', 'baz']);
        $this->assertNull(search($iter, func\operator('===', 'qux')));

        $iter = new \ArrayIterator([]);
        $this->assertNull(search($iter, func\operator('===', 'qux')));
    }

    public function testTakeOrDropWhile(): void
    {
        $this->assertSame(
            [3, 1, 4],
            toArray(takeWhile([3, 1, 4, -1, 5], func\operator('>', 0)))
        );
        $this->assertSame(
            [-1, 5],
            toArray(dropWhile([3, 1, 4, -1, 5], func\operator('>', 0)))
        );
        $this->assertSame(
            [1, 2, 3],
            toArray(takeWhile([1, 2, 3], func\operator('>', 0)))
        );
        $this->assertSame(
            [],
            toArray(dropWhile([1, 2, 3], func\operator('>', 0)))
        );
    }

    public function testFlatten(): void
    {
        $this->assertSame(
            [1, 2, 3, 4, 5],
            toArray(flatten([1, 2, 3, 4, 5]))
        );
        $this->assertSame(
            [1, 2, 3, 4, 5],
            toArray(flatten([1, [2, 3], 4, [], 5]))
        );
        $this->assertSame(
            [1, 2, 3, 4, 5],
            toArray(flatten([1, [[2, 3], 4], 5]))
        );
        $this->assertSame(
            [1, 2, 3, 4, 5],
            toArray(flatten([[1, [[2, [[]], 3], 4]], 5]))
        );
        $this->assertSame(
            [1, 2, 3, 4, 5],
            toArray(flatten(new \ArrayIterator([
                new \ArrayIterator([1, 2]),
                3,
                new \ArrayIterator([4, 5]),
            ])))
        );

        // Test key preservation
        $this->assertSame(
            ['a' => 1, 'c' => 2, 'd' => 3],
            toArrayWithKeys(flatten(['a' => 1, 'b' => ['c' => 2, 'd' => 3]]))
        );
    }

    public function testFlattenLevels(): void
    {
        $this->assertSame(
            [[1, [[2, [[]], 3], 4]], 5],
            toArray(flatten([[1, [[2, [[]], 3], 4]], 5], 0))
        );
        $this->assertSame(
            [1, [[2, [[]], 3], 4], 5],
            toArray(flatten([[1, [[2, [[]], 3], 4]], 5], 1))
        );
        $this->assertSame(
            [1, [2, [[]], 3], 4, 5],
            toArray(flatten([[1, [[2, [[]], 3], 4]], 5], 2))
        );
        $this->assertSame(
            [1, 2, [[]], 3, 4, 5],
            toArray(flatten([[1, [[2, [[]], 3], 4]], 5], 3))
        );
    }
    
    public function testFlattenNegativeLevelError(): void
    {
        $this->expectException(\InvalidArgumentException::class);
        $this->expectExceptionMessage('Number of levels must be non-negative');

        toArray(flatten([1, 2, 3], -1));
    }

    public function testToIter(): void
    {
        $iter = new \ArrayIterator([1, 2, 3]);
        $this->assertSame($iter, toIter($iter));

        $iter = toIter(new \ArrayObject([1, 2, 3]));
        $this->assertInstanceOf('Iterator', $iter);
        $this->assertSame([1, 2, 3], toArray($iter));

        $iter = toIter([1, 2, 3]);
        $this->assertInstanceOf('ArrayIterator', $iter);
        $this->assertSame([1, 2, 3], toArray($iter));

        // DatePeriod is Traversable, but not Iterator or IteratorAggregate
        $iter = toIter(new \DatePeriod(
            new \DateTime('2012-07-01'),
            new \DateInterval('P7D'),
            new \DateTime('2012-07-16')
        ));
        $this->assertInstanceOf('Iterator', $iter);
        $this->assertSame(
            ['2012-07-01', '2012-07-08', '2012-07-15'],
            iterator_to_array(map($iter, static function(\DateTimeInterface $date) {
                return $date->format('Y-m-d');
            }))
        );
    }

    public function testCount(): void
    {
        $this->assertCount(5, [1, 2, 3, 4, 5]);
        $this->assertCount(5, toIter([1, 2, 3, 4, 5]));
        $this->assertCount(42, new _CountableTestDummy);
    }

    public function testIsEmpty(): void
    {
        $this->assertTrue(isEmpty([]));
        $this->assertFalse(isEmpty([null]));
        $this->assertTrue(isEmpty(toArray([])));
        $this->assertFalse(isEmpty(toArray([null])));
        $this->assertTrue(isEmpty(repeat(42, 0)));
        $this->assertFalse(isEmpty(repeat(42)));
    }

    public function testToArray(): void
    {
        $this->assertSame([1, 2, 3], toArray(['a' => 1, 'b' => 2, 'c' => 3]));
        $this->assertSame(
            [1, 2, 3],
            toArray(new \ArrayIterator(['a' => 1, 'b' => 2, 'c' => 3]))
        );
        $this->assertSame(
            [1, 2, 3],
            toArray(chain(['a' => 1, 'b' => 2], ['a' => 3]))
        );
    }

    public function testFlip(): void
    {
        $this->assertSame(
            [1 => 'a', 2 => 'b', 3 => 'c'],
            toArrayWithKeys(flip(['a' => 1, 'b' => 2, 'c' => 3]))
        );
    }

    public function testJoin(): void
    {
        $this->assertSame('', join(', ', []));
        $this->assertSame(
            'a, b, c',
            join(', ', new \ArrayIterator(['a', 'b', 'c']))
        );
    }

    public function testSplit(): void
    {
        $this->assertSame(['a', 'b', 'c'], toArray(split(', ', 'a, b, c')));
        $this->assertSame(['b', 'b', 'b', 'b', 'b', 'b', 'b'], toArray(split('a', 'babababababab')));

        $this->assertSame(['a', 'b', '', '', 'c'], toArray(split(',', 'a,b,,,c')));
        $this->assertSame(['', '', 'c'], toArray(split(',', ',,c')));
        $this->assertSame(['c', '', ''], toArray(split(',', 'c,,')));

        $this->expectException(\InvalidArgumentException::class);
        split('', 'a');
    }

    public function testChunk(): void
    {
        $iterable = new \ArrayIterator(
            ['a' => 1, 'b' => 2, 'c' => 3, 'd' => 4, 'e' => 5]
        );

        $this->assertSame(
            [[1, 2], [3, 4], [5]],
            toArray(chunk($iterable, 2))
        );
        $this->assertSame(
            [[0, 1], [2, 3]],
            toArray(chunk([0, 1, 2, 3], 2))
        );

        $this->assertSame([[0, 1, 2]], toArray(chunk([0, 1, 2], 100000)));
        $this->assertSame([], toArray(chunk([], 100000)));

        $this->assertSame(
            [['a' => 1, 'b' => 2], ['c' => 3, 'd' => 4], ['e' => 5]],
            toArray(chunk($iterable, 2, true))
        );
        $this->assertSame(
            [[0=>0, 1=>1], [2=>2, 3=>3]],
            toArray(chunk([0, 1, 2, 3], 2, true))
        );

        $this->assertSame(
            [['a' => 1, 'b' => 2], ['c' => 3, 'd' => 4], ['e' => 5]],
            toArray(chunkWithKeys($iterable, 2))
        );
        $this->assertSame(
            [[0=>0, 1=>1], [2=>2, 3=>3]],
            toArray(chunkWithKeys([0, 1, 2, 3], 2))
        );
    }

    
    public function testZeroChunkSizeError(): void
    {
        $this->expectException(\InvalidArgumentException::class);
        $this->expectExceptionMessage('Chunk size must be positive');

        toArray(chunk([1, 2, 3], 0));
    }

    
    public function testNegativeChunkSizeError(): void
    {
        $this->expectException(\InvalidArgumentException::class);
        $this->expectExceptionMessage('Chunk size must be positive');

        toArray(chunk([1, 2, 3], -1));
    }

    public function testProduct(): void
    {
        $this->assertKeysValues([[]], [[]], static function() { return product(); });

        $this->assertKeysValues(
            [[0],[1]], [[1],[2]], static function() { return product([1,2]); });

        $this->assertKeysValues(
            [[0,0],[0,1],[1,0],[1,1]],
            [[1,3],[1,4],[2,3],[2,4]],
            static function() { return product([1,2],[3,4]); });

        $this->assertKeysValues(
            [[0,0,0],[0,0,1],[0,1,0],[0,1,1],[1,0,0],[1,0,1],[1,1,0],[1,1,1]],
            [[1,1,1],[1,1,2],[1,2,1],[1,2,2],[2,1,1],[2,1,2],[2,2,1],[2,2,2]],
            static function() {
                return product(range(1,2), [1,2], new \ArrayIterator([1,2]));
            }
        );
    }

    function testRecurse(): void
    {
        $iter = new \ArrayIterator(['a' => 1, 'b' => 2,
            'c' => new \ArrayIterator(['d' => 3, 'e' => 4])]);

        $this->assertSame(
            [1, 2, [3, 4]],
            recurse($iter, 'iter\toArray')
        );

        $this->assertSame(
            ['a' => 1, 'b' => 2, 'c' => ['d' => 3, 'e' => 4]],
            recurse($iter, 'iter\toArrayWithKeys')
        );
    }

    private function assertKeysValues(array $keys, array $values, callable $fn): void
    {
        $this->assertSame($keys, toArray(keys($fn())));
        $this->assertSame($values, toArray(values($fn())));
    }

    public function testIsIterable(): void
    {
        $this->assertTrue(isIterable([]));
        $this->assertTrue(isIterable([1, 2, 3]));
        $this->assertTrue(isIterable(new \ArrayIterator([1, 2, 3])));
        $gen = static function() { yield; };
        $this->assertTrue(isIterable($gen()));

        $this->assertFalse(isIterable(new \stdClass()));
        $this->assertFalse(isIterable('foobar'));
        $this->assertFalse(isIterable(123));
    }

    /**
     * @dataProvider provideTestAssertIterableFails
     */
    public function testAssertIterableFails(callable $fn, $expectedMessage, $expectedException): void
    {
        if(null !== $expectedMessage){
            $this->expectExceptionMessage($expectedMessage);
        }
        $this->expectException($expectedException);
        $ret = $fn();

        // For generators the body will not be run until the first operation
        if ($ret instanceof \Generator) {
            $ret->rewind();
        }
    }

    public function provideTestAssertIterableFails(): ?\Generator
    {
        yield [
            static function() { return count(new \stdClass()); },
            'Argument must be iterable or implement Countable',
            \InvalidArgumentException::class
        ];
        yield [
            static function() { return isEmpty(new \stdClass()); },
            'Argument must be iterable or implement Countable',
            \InvalidArgumentException::class
        ];
        yield [
            static function() { return toIter(new \stdClass()); },
            null,
            \TypeError::class
        ];
        yield [
            static function() {
                return map(new \stdClass(), static function($v) { return $v; });
            },
            null,
            \TypeError::class
        ];
        yield [
            static function() {
                return chain([1], [2], new \stdClass());
            },
            null,
            \TypeError::class
        ];
        yield [
            static function() {
                return zip([1], [2], new \stdClass());
            },
            null,
            \TypeError::class
        ];
    }

    /**
     * @dataProvider provideTestMapUsingKeysData
     * @param iterable $input
     * @param callable $callback
     * @param array $expected
     */
    public function testMapWithKeys(iterable $input, callable $callback, array $expected): void
    {
        self::assertEquals(toArray(mapWithKeys($input, $callback)), $expected);
    }

    public function provideTestMapUsingKeysData(): array
    {
        return [
            'Empty array' => [
                'input' => [],
                'callable' => static function($key, $value) {
                    return [
                        'id' => $key,
                        'name' => $value,
                    ];
                },
                'expected' => [],
            ],
            'Array with keys and values' => [
                'input' => [3 => 'John', 8 => 'Dough', 1 => 'James'],
                'callable' => static function($key, $value) {
                    return [
                        'id' => $key,
                        'name' => $value,
                    ];
                },
                'expected' => [
                    [
                        'id' => 3,
                        'name' => 'John',
                    ],
                    [
                        'id' => 8,
                        'name' => 'Dough',
                    ],
                    [
                        'id' => 1,
                        'name' => 'James',
                    ],
                ],
            ],
            'Traversable with keys and values' => [
                'input' => (static function() {
                    yield 3 => 'John';
                    yield 8 => 'Dough';
                    yield 1 => 'James';
                })(),
                'callable' => static function($key, $value) {
                    return [
                        'id' => $key,
                        'name' => $value,
                    ];
                },
                'expected' => [
                    [
                        'id' => 3,
                        'name' => 'John',
                    ],
                    [
                        'id' => 8,
                        'name' => 'Dough',
                    ],
                    [
                        'id' => 1,
                        'name' => 'James',
                    ],
                ],
            ],
        ];
    }

    /**
     * @dataProvider provideTestMapWithKeysData
     * @param iterable $input
     * @param callable $callback
     * @param array $expected
     */
    public function testMapKeyVals(iterable $input, callable $callback, array $expected): void
    {
        $actual = iterator_to_array(mapKeyVals($input, $callback));
        sort($actual);
        sort($expected);
        self::assertSame($actual, $expected);
    }

    public function provideTestMapWithKeysData(): array
    {
        return [
            'Empty array' => [
                'input' => [],
                'callable' => static function($item) {
                    return [$item['id'], $item['name']];
                },
                'expected' => [],
            ],
            'Some array values' => [
                'input' => [
                    [
                        'id' => 3,
                        'name' => 'John',
                    ],
                    [
                        'id' => 8,
                        'name' => 'Dough',
                    ],
                    [
                        'id' => 1,
                        'name' => 'James',
                    ],
                ],
                'callable' => static function($item) {
                    return [$item['id'], $item['name']];
                },
                'expected' => [3 => 'John', 8 => 'Dough', 1 => 'James'],
            ],
            'Traversable settings non-unique values as keys' => [
                'input' => (static function() {
                    yield [
                        'id' => 8,
                        'name' => 'John',
                    ];
                    yield [
                        'id' => 8,
                        'name' => 'Dough',
                    ];
                    yield [
                        'id' => 1,
                        'name' => 'James',
                    ];
                })(),
                'callable' => static function($item) {
                    return [$item['id'], $item['name']];
                },
                'expected' => [8 => 'Dough', 1 => 'James'],
            ],
        ];
    }

    /**
     * @dataProvider provideIterFilterData
     * @param iterable $input
     * @param callable $callback
     * @param array $expected
     */
    public function testFilter(iterable $input, callable $callback, array $expected): void
    {
        self::assertEquals(toArray(filter($input, $callback)), $expected);
    }

    public function provideIterFilterData(): array
    {
        return [
            'Empty array' => [
                'input' => [],
                'callable' => static function($item) {
                    return $item > 8;
                },
                'expected' => [],
            ],
            'Array with integers' => [
                'input' => [1, 8, 3, 5, 1, 4, 9, 2, 6, 4, 7, 23, 9, 10],
                'callable' => static function($item) {
                    return $item > 8;
                },
                'expected' => [9, 23, 9, 10],
            ],
            'Iterator with integers' => [
                'input' => range(-5, 5),
                'callable' => static function($item) {
                    return $item < 0;
                },
                'expected' => [-5, -4, -3, -2, -1],
            ],
            'Iterator with integers and keys' => [
                'input' => (static function() {
                    yield 6 => 300;
                    yield 3 => 2;
                    yield 1 => 6;
                    yield 13 => 9;
                    yield 'bla' => 300;
                })(),
                'callable' => static function($item) {
                    return $item > 8;
                },
                'expected' => [6 => 300, 13 => 9, 'bla' => 300],
            ],
            'Array with strings' => [
                'input' => ['13', '', '15', 'fifteen', '1', 'some random string'],
                'callable' => static function($item) {
                    return strlen($item) > 1;
                },
                'expected' => ['13', '15', 'fifteen', 'some random string'],
            ],
        ];
    }

    /**
     * @dataProvider provideTestMapData
     * @param iterable $input
     * @param callable $callback
     * @param array $expected
     */
    public function testMap(iterable $input, callable $callback, array $expected): void
    {
        $actual = iterator_to_array(map($input, $callback));
        sort($actual);
        sort($expected);
        self::assertSame($actual, $expected);
    }

    public function provideTestMapData(): array
    {
        return [
            'Empty array' => [
                'input' => [],
                'callable' => static function($item) {
                    return $item['a'];
                },
                'expected' => [],
            ],
            'Array' => [
                'input' => [['a' => 3], ['a' => 4], ['a' => 3]],
                'callable' => static function($item) {
                    return $item['a'];
                },
                'expected' => [3, 4, 3],
            ],
            'Iterator with values' => [
                'input' => range(0, 5),
                'callable' => static function($value) {
                    return $value * 3;
                },
                'expected' => [0, 3, 6, 9, 12, 15],

            ],
            'Iterator with keys' => [
                'input' => (static function() {
                    yield 'asd' => ['a' => 3];
                    yield 'someKey' => ['a' => 4];
                    yield ['a' => 3];
                })(),
                'callable' => static function($item) {
                    return $item['a'];
                },
                'expected' => ['asd' => 3, 'someKey' => 4, 3],
            ],
            'Array with unused key-values and callback with nested statement' => [
                'input' => [
                    [
                        'bla' => 8,
                        'children' => [
                            [
                                'uuid' => 3,
                                'someOtherKey' => 9
                            ],
                            'plop' => 9,
                        ]
                    ],
                    [
                        'bla' => 8,
                        'children' => [
                            [
                                'someOtherKey' => 9,
                                'uuid' => 4
                            ],
                            'plop' => 9,
                        ]
                    ],
                    [
                        'bla' => 8,
                        'children' => [
                            [
                                'uuid' => 3,
                                'someOtherKey' => 9
                            ],
                            'plop' => 9,
                        ]
                    ],
                ],
                'callable' => static function($item) {
                    return $item['children'][0]['uuid'];
                },
                'expected' => [3, 4, 3],
            ],
        ];
    }

    /**
     * @dataProvider provideTestIterToArrayData
     * @param iterable $input
     * @param array $expected
     */
    public function testIterToArray(iterable $input, array $expected): void
    {
        self::assertEquals(toArrayWithKeys($input), $expected);
    }

    public function provideTestIterToArrayData(): array
    {
        return [
            'empty array' => [
                'input' => [],
                'expected' => [],
            ],
            'empty Traversable' => [
                'input' => (static function() {
                    if (false) {
                        yield null;
                    }
                })(),
                'expected' => [],
            ],
            'non-empty array' => [
                'input' => [1, 4, 9],
                'expected' => [1, 4, 9],
            ],
            'non-empty Traversable' => [
                'input' => (static function() {
                    yield 13;
                    yield 5;
                    yield 19;
                })(),
                'expected' => [13, 5, 19],
            ],
            'array with containing different types' => [
                'input' => [[], [1, 3, 5], 5, 7, [8 => 9, 10], '14' => 16, 3 => [1], 71 => '3'],
                'expected' => [[], [1, 3, 5], 5, 7, [8 => 9, 10], '14' => 16, 3 => [1], 71 => '3'],
            ],
            'Traversable with containing different types' => [
                'input' => (static function() {
                    yield [];
                    yield [1, 3, 5];
                    yield 5;
                    yield 7;
                    yield [8 => 9, 10];
                    yield '14' => 16;
                    yield 3 => [1];
                    yield 71 => '3';
                })(),
                'expected' => [[], [1, 3, 5], 5, 7, [8 => 9, 10], '14' => 16, 3 => [1], 71 => '3'],
            ],
        ];
    }

    /**
     * @dataProvider provideTestSomeData
     * @param iterable $input
     * @param callable $callback
     * @param bool $expected
     */
    public function testAny(iterable $input, callable $callback, bool $expected): void
    {
        self::assertEquals(any($input, $callback), $expected);
    }

    public function provideTestSomeData(): array
    {
        return [
            'empty array' => [
                'input' => [],
                'callable' => static function(int $num) {
                    return $num < 10;
                },
                'expected' => false,
            ],
            'Contains a value that matches the predicate' => [
                'input' => [13, 93, 32, 10, 3, 100],
                'callable' => static function(int $num) {
                    return $num < 10;
                },
                'expected' => true,
            ],
            'Contains no value that matches the predicate' => [
                'input' => [13, 93, 32, 10, 30, 100],
                'callable' => static function(int $num) {
                    return $num < 10;
                },
                'expected' => false,
            ],
        ];
    }

}

class _CountableTestDummy implements \Countable {
    public function count() {
        return 42;
    }
}

<?php
namespace cerbere\PHPUnit;

class SimpleTest
{
	public function divide()
	{
		$simple = new Simple(10);
		$result = $simple->divide(2);
	}

    /**
     * @expectedException \InvalidArgumentException
     */
    public function divideWithException()
    {
        $simple = new Simple(10);
        $simple->divide(0);
    }
}

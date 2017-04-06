
backup:
	mv spec/output.xml spec/output.xml.old

diff:
	diff -u spec/output.xml.old spec/output.xml | less


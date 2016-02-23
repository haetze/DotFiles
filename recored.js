/*
 * recored.js
 * Copyright (C) 2016 haetze <haetze@Richards-MacBook-Air.local>
 *
 * Distributed under terms of the MIT license.
 */



var spawn = require('child_process').exec;
var execString = 'mpg123 -O $(date "+%Y-%m-%d-%H:%M:%S").info  -C --wav $(date "+%Y-%m-%d-%H:%M:%S").wav http://1live.akacast.akamaistream.net/7/706/119434/v1/gnl.akacast.akamaistream.net/1live'
var options = { encoding : 'utf8',
		timeout : 30,
		maxBuffer : 200*1024,
		killSignal : 'SIGTERM',
		cwd : null,
		env : null}

var child =  spawn(execString);

setInterval(spawner, 1000*5*60);


function spawner(){
    child.kill('SIGKILL');
    child = spawn(execString);

}


//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//

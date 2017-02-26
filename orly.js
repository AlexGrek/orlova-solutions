
window.onload = function() {
    



print = function(targetid) {
    var element = document.getElementById(targetid)
    if (element == null || element == undefined)
        console.log ("Element is ", element)
    else 
        console.log ("It's ok, element is ", element)
    return function () {
        var args = Array.prototype.slice.call(arguments);
        var node = document.createElement("P");
        var str = "";
	    args.forEach(function(arg) {
            str += arg + " ";
	    });
        var textnode = document.createTextNode(str);
        node.appendChild(textnode);
        element.appendChild(node);
    }
}

consprint = print("microconsole")

createTable = function (headers, values) {
    var table = document.createElement("table")
    var i = 0;
    var tr = document.createElement("tr")
    for (i = 0; i < headers.length; i++) {
        var th = document.createElement("th")
        var text = document.createTextNode(headers[i])
        th.appendChild(text)
        tr.appendChild(th)
    }
    table.appendChild(tr)

    for (i = 0; i < values.length; i++) {
        var tr = document.createElement("tr")
        var j = 0;
        for (j = 0; j < headers.length; j++) {
            var th = document.createElement("td")
            var text = document.createTextNode(values[i][j])
            th.appendChild(text)
            tr.appendChild(th)
        }
        table.appendChild(tr)
    }
    return table;
}

printTable = function (table) {
    var place = document.getElementById("microconsole")
    console.log(table)
    place.appendChild(table)
} 

printDatagram = function (d) {
    consprint("----------IP Datagram----------")
    consprint("Size:         ", d.size)
    consprint("Payload size: ", d.payloadSize)
    consprint("Offset:       ", d.offset)
    consprint("Contents:     ", d.payloadFrom, "to", d.payloadTo)
}

datagramsToTable = function(ds) {
    var headers = ["Size", "PayloadSize", "Offset", "Contents", "IsNotLast", "TTL"]
    var i = 0;
    var items = []
    for (i = 0; i < ds.length; i++) {
        var contents = "From " + ds[i].payloadFrom + " to " + ds[i].payloadTo;
        var item = [ds[i].size, ds[i].payloadSize, ds[i].offset,
            contents, ds[i].isNotLast, ds[i].ttl];
        items.push(item)
    }
    console.log(items)
    return createTable(headers, items)
}

getMaxPossiblePayload = function (mtu) {
    var maxload = mtu - 20 // minus IP header
    console.log("maximum payload (mtu-20) = ", maxload)
    while (maxload % 8 != 0) {
        maxload -= 1;   // maximum possible payload must be / 8
    }
    console.log("maximum real payload = ", maxload)
    return maxload;
}

calculate = function() {
    var msize = parseInt(document.getElementById('msize').value);
    var ttl = parseInt(document.getElementById('ttl').value);
    var hsize = parseInt(document.getElementById('hsize').value);
    var mtu = parseInt(document.getElementById('mtu').value);
    console.log("msize = ", msize)
    console.log("mtu = ", mtu)
    console.log("hsize = ", hsize)
    if (!Number.isInteger(hsize))
        hsize = 0;
    if (!Number.isInteger(ttl))
        hsize = 255;
    consprint("Message size (including header):", msize, "MTU:", mtu, "Header:", hsize);
    var maxPayload = getMaxPossiblePayload(mtu);
    var maxDatagramSize = maxPayload + 20;
    var fragments = Math.ceil(msize / maxPayload);
    consprint("Maximum payload per datagram is", maxPayload)
    consprint("Maximum datagram size is", maxDatagramSize)
    consprint("Total fragments:", fragments)
    var datagrams = []  //all datagrams
    var data = msize; //remaining data
    var offset = 0; //datagram offset
    var bytes = 0 - hsize;

    currentTTL = ttl;
    if (fragments > 0)  //because we have to split it
        currentTTL -= 1

    while (fragments > 0) {
        fragments--;
        var datagram = {} // create new datagram object
        datagram.offset = offset;
        if (fragments == 0) {   //if this is the last datagram
            datagram.size = data + 20; // + IP header
            datagram.payloadSize = data;
            datagram.isNotLast = 0;
            
        } else {
            datagram.size = maxPayload + 20; // + IP header
            datagram.payloadSize = maxPayload;
            datagram.isNotLast = 1;
        }
        datagram.payloadFrom = bytes + 1; // because we count bytes from 1
        datagram.payloadTo = bytes + datagram.payloadSize;

        //add common data
        datagram.ttl = currentTTL;

        data -= datagram.payloadSize;
        offset += datagram.payloadSize / 8;
        bytes += datagram.payloadSize;
        datagrams.push(datagram)
        //printDatagram(datagram)
    }

    var dgs = datagramsToTable(datagrams)
    printTable(dgs)
}

};
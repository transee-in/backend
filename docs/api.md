### api documentation

---

Current API version have only `yaroslavl`.

---

#### city information

City information method contains list of transports in city

```
{
  transport_type -> {
    transport_id -> transport_name
  }
}
```

```http
GET /api/<city>

{
  "autobus": {
    "2": "2",
    "21t": "21т",
    "44m": "44м",
    "93g": "93г"
  },
  "trolleybus": {
    "1": "1",
    "2": "2",
  },
  "tram": {
    "1": "1",
    "5": "5",
    "6": "6",
    "7": "7"
  }
}
```

#### city transport routes

Method for get transport routes of the city, server responds in format:

```
{
  transport_type -> {
    transport_id -> [latitude, longitude]
  }
}
```

```http
GET /api/<city>/routes

{
  "autobus": {
    "2": [
      [57.642743, 39.873551],
      [57.642502, 39.873647],
      [57.642145, 39.874366],
      [57.640517, 39.874967],
      [57.640143, 39.875289],
      ...
    ],
    ...
  },
  ...
}
```

#### city transport stations

If you want to get transport stations coordinates use this method:

```
{
  transport_type -> [
    transport_id -> [latitude, longitude]
  ]
}
```

```http
GET /api/<city>/routes

{
  "autobus": [
    [57.630056, 39.850486],
    [57.626954, 39.842537],
    [57.625810, 39.836146],
    [57.627042, 39.843681],
    [57.629375, 39.849556],
    ...
  ],
  ...
}
```

#### city all transport positions

To get all transport positions:

```
{
  transport_type -> {
    transport_id -> [latitude, longitude, angle]
  }
}
```

```http
GET /api/<city>/positions

{
  "autobus": {
    "2": [
      57.625783,
      39.846067,
      170
    ],
    ...
  }
  ...
}
```

Also, you can filter response by transport type:

```http
GET /api/<city>/positions?type=trolleybus

{
  "trolleybus": {
    "2": [
      57.625783,
      39.846067,
      170
    ],
    ...
  }
}
```

And by transport numbers:


```http
GET /api/<city>/positions?type=trolleybus&numbers[]=2

{
  "trolleybus": {
    "2": [
      57.625783,
      39.846067,
      170
    ]
  }
}
```

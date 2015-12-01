<?php

// Current Namespace: My
namespace My;

// Namespace: My\Project
// Type name: Type
// Alias:     Type
use My\Project\Type;

// Namespace: \
// Type name: DateTime
// Alias:     DateTime
use DateTime;

// Namespace: Carbon
// Type name: Carbon
// Alias:     CarbonDate
use Carbon\Carbon as CarbonDate;

$carbonDate = new CarbonDate();

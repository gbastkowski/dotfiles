#!/usr/bin/env sh

## Collect data
cache_dir="$HOME/.cache/eww/weather"
cache_weather_stat=${cache_dir}/weather-stat
cache_weather_degree=${cache_dir}/weather-degree
cache_weather_quote=${cache_dir}/weather-quote
cache_weather_hex=${cache_dir}/weather-hex
cache_weather_icon=${cache_dir}/weather-icon

## Weather data
KEY="YOUR_KEY"
ID="CITY_ID"
UNIT="metric"

LAT=52.446562
LON=13.331812
APPID=`pass show private/openweather/api-key`

## Make cache dir
if [[ ! -d "$cache_dir" ]]; then
    mkdir -p ${cache_dir}
fi

## Get data
get_weather_data() {
    weather=`curl -sf https://api.openweathermap.org/data/2.5/weather?units=metric\&lat=${LAT}\&lon=${LON}\&appid=${APPID}`
    echo ${weather}

    if [ ! -z "$weather" ]; then
        weather_temp=`echo "$weather" | jq '.main.temp'`
        weather_icon_code=`echo "$weather" | jq -r ".weather[].icon" | head -1`
        weather_description=`echo "$weather" | jq -r ".weather[].description" | head -1 | sed -e "s/\b\(.\)/\u\1/g"`

        #Big long if statement of doom
	if [ "$weather_icon_code" == "50d"  ]; then
            weather_icon="  "
	    weather_quote="Forecast says it's misty"
	    weather_hex="#84afdb"
        elif [ "$weather_icon_code" == "50n"  ]; then
            weather_icon="  "
            weather_quote="Forecast says it's a misty night"
            weather_hex="#84afdb"
        elif [ "$weather_icon_code" == "01d"  ]; then
            weather_icon="  "
            weather_quote="It's a sunny day, gonna be fun!"
            weather_hex="#ffd86b"
        elif [ "$weather_icon_code" == "01n"  ]; then
            weather_icon="  "
            weather_quote="It's a clear night"
            weather_hex="#fcdcf6"
        elif [ "$weather_icon_code" == "02d"  ]; then
            weather_icon="  "
            weather_quote="It's  cloudy, sort of gloomy"
            weather_hex="#adadff"
        elif [ "$weather_icon_code" == "02n"  ]; then
            weather_icon="  "
            weather_quote="It's a cloudy night"
            weather_hex="#adadff"
        elif [ "$weather_icon_code" == "03d"  ]; then
            weather_icon="  "
            weather_quote="It's  cloudy, sort of gloomy"
            weather_hex="#adadff"
        elif [ "$weather_icon_code" == "03n"  ]; then
            weather_icon="  "
            weather_quote="It's a cloudy night"
            weather_hex="#adadff"
        elif [ "$weather_icon_code" == "04d"  ]; then
            weather_icon="  "
            weather_quote="It's  cloudy, sort of gloomy"
            weather_hex="#adadff"
        elif [ "$weather_icon_code" == "04n"  ]; then
            weather_icon="  "
            weather_quote="It's a cloudy night"
            weather_hex="#adadff"
        elif [ "$weather_icon_code" == "09d"  ]; then
            weather_icon="  "
            weather_quote="It's rainy, it's a great day!"
            weather_hex="#6b95ff"
        elif [ "$weather_icon_code" == "09n"  ]; then
            weather_icon="  "
            weather_quote=" It's gonna rain tonight it seems \n"
            weather_hex="#6b95ff"
        elif [ "$weather_icon_code" == "10d"  ]; then
            weather_icon="  "
            weather_quote="It's rainy, it's a great day!"
            weather_hex="#6b95ff"
        elif [ "$weather_icon_code" == "10n"  ]; then
            weather_icon="  "
            weather_quote=" It's gonna rain tonight it seems"
            weather_hex="#6b95ff"
        elif [ "$weather_icon_code" == "11d"  ]; then
            weather_icon="  "
            weather_quote="There's storm for forecast today"
            weather_hex="#ffeb57"
        elif [ "$weather_icon_code" == "11n"  ]; then
            weather_icon="  "
            weather_quote="There's gonna be storms tonight"
            weather_hex="#ffeb57"
        elif [ "$weather_icon_code" == "13d"  ]; then
            weather_icon="  "
            weather_quote="It's gonna snow today"
            weather_hex="#e3e6fc"
        elif [ "$weather_icon_code" == "13n"  ]; then
            weather_icon="  "
            weather_quote="It's gonna snow tonight"
            weather_hex="#e3e6fc"
        elif [ "$weather_icon_code" == "40d"  ]; then
            weather_icon="  "
            weather_quote="Forecast says it's misty"
            weather_hex="#84afdb"
        elif [ "$weather_icon_code" == "40n"  ]; then
            weather_icon="  "
            weather_quote="Forecast says it's a misty night"
            weather_hex="#84afdb"
        else
            weather_icon="  "
            weather_quote="Sort of odd, I don't know what to forecast"
            weather_hex="#adadff"
        fi
        echo "$weather_icon"        > ${cache_weather_icon}
        echo "$weather_description" > ${cache_weather_stat}
        echo "$weather_temp""°C"    > ${cache_weather_degree}
        echo -e "$weather_quote"    > ${cache_weather_quote}
        echo "$weather_hex"         > ${cache_weather_hex}
    else
        echo "Weather Unavailable"  > ${cache_weather_stat}
        echo " "                  > ${cache_weather_icon}
        echo -e "Ah well, no weather huh? \nEven if there's no weather, it's gonna be a great day!" > ${cache_weather_quote}
        echo "-"                    > ${cache_weather_degree}
        echo "#adadff"              > ${cache_weather_hex}
    fi
}

## Execute
if [[ "$1" == "--getdata"  ]];  then get_weather_data
elif [[ "$1" == "--icon"   ]];  then cat ${cache_weather_icon}
elif [[ "$1" == "--temp"   ]];  then cat ${cache_weather_degree}
elif [[ "$1" == "--hex"    ]];  then cat ${cache_weather_hex}
elif [[ "$1" == "--stat"   ]];  then cat ${cache_weather_stat}
elif [[ "$1" == "--quote"  ]];  then cat ${cache_weather_quote} | head -n1
elif [[ "$1" == "--quote2" ]];  then cat ${cache_weather_quote} | tail -n1
fi
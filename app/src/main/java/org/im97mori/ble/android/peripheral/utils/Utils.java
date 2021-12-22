package org.im97mori.ble.android.peripheral.utils;


import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.reflect.TypeToken;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.DescriptorData;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.UUID;

public final class Utils {

    private Utils() {
    }

    private static class CharacteristicDataDeserializer implements JsonDeserializer<CharacteristicData> {
        private static final Type DESCRIPTOR_LIST_TYPE = new TypeToken<ArrayList<DescriptorData>>() {
        }.getType();

        @Override
        public CharacteristicData deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
            JsonObject jsonObject = json.getAsJsonObject();
            UUID uuid = UUID.fromString(jsonObject.get("uuid").getAsString());
            CharacteristicData data;
//            if (CharacteristicUUID.EMAIL_ADDRESS_CHARACTERISTIC.equals(uuid)) {
//                data = context.deserialize(json, CharacteristicData2.class);
//            } else {
            data = new CharacteristicData(uuid
                    , context.deserialize(jsonObject.get("property"), int.class)
                    , context.deserialize(jsonObject.get("permission"), int.class)
                    , context.deserialize(jsonObject.get("descriptor_data_list"), DESCRIPTOR_LIST_TYPE)
                    , context.deserialize(jsonObject.get("response_code"), int.class)
                    , context.deserialize(jsonObject.get("delay"), long.class)
                    , context.deserialize(jsonObject.get("data"), byte[].class)
                    , context.deserialize(jsonObject.get("notification_count"), int.class));
//            }
            return data;
        }

    }

    private static Gson mGson;

    public synchronized static Gson createGsonInstance() {
        if (mGson == null) {
            mGson = new GsonBuilder()
                    .registerTypeAdapter(CharacteristicData.class, new CharacteristicDataDeserializer())
                    .create();
        }
        return mGson;
    }

}

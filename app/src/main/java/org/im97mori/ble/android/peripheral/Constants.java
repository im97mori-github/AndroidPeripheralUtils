package org.im97mori.ble.android.peripheral;

public class Constants {

    private Constants() {
    }

    public static class DeviceTypes {
        public static final int DEVICE_TYPE_UNDEFINED = 0x00;
        public static final int DEVICE_TYPE_BLOOD_PRESSURE_PROFILE = 0x01;
    }

    public static class IntentKey {

        public static final String KEY_DEVICE_TYPE = "KEY_DEVICE_TYPE";

        public static final String KEY_DEVICE_ID = "KEY_DEVICE_ID";
        public static final long VALUE_DEVICE_ID_UNSAVED = -1;

        public static final String KEY_PROPERTIES_TYPE = "KEY_PROPERTIES_TYPE";
    }
}

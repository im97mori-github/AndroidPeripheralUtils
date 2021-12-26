package org.im97mori.ble.android.peripheral.room;

import androidx.room.Database;
import androidx.room.RoomDatabase;

@Database(entities = {DeviceSetting.class}, version = 1, exportSchema = false)
public abstract class AppDatabase extends RoomDatabase {

    public abstract DeviceSettingDao getDeviceSettingDao();

}

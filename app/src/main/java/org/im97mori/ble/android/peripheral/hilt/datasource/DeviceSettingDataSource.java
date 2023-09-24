package org.im97mori.ble.android.peripheral.hilt.datasource;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.room.Room;

import org.im97mori.ble.android.peripheral.room.AppDatabase;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;

import java.util.List;

import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Observable;
import io.reactivex.rxjava3.core.Single;

public class DeviceSettingDataSource {

    private final AppDatabase mAppDatabase;

    private static final String DB_NAME = "app_database.db";

    public DeviceSettingDataSource(@NonNull Context context) {
        mAppDatabase = Room.databaseBuilder(context.getApplicationContext()
                , AppDatabase.class
                , DB_NAME)
                .fallbackToDestructiveMigration()
                .build();
    }

    @NonNull
    public Observable<List<DeviceSetting>> loadAllDeviceSetting() {
        return mAppDatabase.getDeviceSettingDao().loadAllDeviceSetting();
    }

    @NonNull
    public Single<DeviceSetting> loadDeviceSettingById(long id) {
        return mAppDatabase.getDeviceSettingDao().loadDeviceSettingById(id);
    }

    @NonNull
    public Completable insertDeviceSetting(@NonNull DeviceSetting deviceSetting) {
        return mAppDatabase.getDeviceSettingDao().insertDeviceSetting(deviceSetting);
    }

    @NonNull
    public Completable deleteDeviceSetting(@NonNull DeviceSetting deviceSetting) {
        return mAppDatabase.getDeviceSettingDao().deleteDeviceSetting(deviceSetting);
    }

    @NonNull
    public Completable deleteAllDeviceSetting() {
        return mAppDatabase.getDeviceSettingDao().deleteAllDeviceSetting();
    }

}

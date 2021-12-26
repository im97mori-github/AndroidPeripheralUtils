package org.im97mori.ble.android.peripheral.hilt.datasource;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.room.Room;

import org.im97mori.ble.android.peripheral.room.AppDatabase;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;

import java.util.List;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.core.Single;

public class DeviceDataSource {

    private final Context mApplicationContext;

    private AppDatabase mAppDatabase;

    private static final String DB_NAME = "app_database.db";

    @Inject
    public DeviceDataSource(@NonNull @ApplicationContext Context context) {
        mApplicationContext = context.getApplicationContext();
    }

    private synchronized void initDatabase() {
        if (mAppDatabase == null) {
            mAppDatabase = Room.databaseBuilder(
                    mApplicationContext,
                    AppDatabase.class,
                    DB_NAME
            ).fallbackToDestructiveMigration().build();
        }
    }

    @NonNull
    public Flowable<List<DeviceSetting>> loadAllDeviceSettings() {
        initDatabase();
        return mAppDatabase.getDeviceSettingDao().loadAllDeviceSettings();
    }

    public Single<DeviceSetting> loadDeviceSettingByIdAsSingle(long id) {
        initDatabase();
        return mAppDatabase.getDeviceSettingDao().loadDeviceSettingByIdAsSingle(id);
    }

    public Flowable<DeviceSetting> loadDeviceSettingByIdAsFlowable(long id) {
        initDatabase();
        return mAppDatabase.getDeviceSettingDao().loadDeviceSettingByIdAsFlowable(id);
    }

    @NonNull
    public Completable insertDeviceSetting(@NonNull DeviceSetting deviceSetting) {
        initDatabase();
        return mAppDatabase.getDeviceSettingDao().insertDeviceSetting(deviceSetting);
    }

    @NonNull
    public Completable deleteDeviceSetting(@NonNull DeviceSetting deviceSetting) {
        initDatabase();
        return mAppDatabase.getDeviceSettingDao().deleteDeviceSetting(deviceSetting);
    }

    @NonNull
    public Completable deleteAllDeviceSettings() {
        initDatabase();
        return mAppDatabase.getDeviceSettingDao().deleteAllDeviceSettings();
    }

}

package org.im97mori.ble.android.peripheral.hilt.datasource;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.room.Room;

import org.im97mori.ble.android.peripheral.room.AppDatabase;
import org.im97mori.ble.android.peripheral.room.Device;

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
    public Flowable<List<Device>> loadDevices() {
        initDatabase();
        return mAppDatabase.getDeviceDao().loadDevices().distinct();
    }

    public Single<Device> loadDeviceById(long id) {
        initDatabase();
        return mAppDatabase.getDeviceDao().loadDeviceById(id);
    }

    public Flowable<Device> loadDeviceByIdFlowable(long id) {
        initDatabase();
        return mAppDatabase.getDeviceDao().loadDeviceByIdFlowable(id);
    }

    @NonNull
    public Completable insertDevice(@NonNull Device device) {
        initDatabase();
        return mAppDatabase.getDeviceDao().insertDevice(device);
    }

    @NonNull
    public Completable deleteDevice(@NonNull Device device) {
        initDatabase();
        return mAppDatabase.getDeviceDao().deleteDevice(device);
    }

    @NonNull
    public Completable deleteAllDevices() {
        initDatabase();
        return mAppDatabase.getDeviceDao().deleteAllDevices();
    }

}

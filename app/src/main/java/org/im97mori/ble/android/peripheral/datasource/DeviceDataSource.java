package org.im97mori.ble.android.peripheral.datasource;

import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.util.Pair;
import androidx.room.Room;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.persistence.AppDatabase;
import org.im97mori.ble.android.peripheral.persistence.Device;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.core.Single;

public class DeviceDataSource {

    private final Context mApplicationContext;

    private AppDatabase mAppDatabase;

    private static final String mDBName = "test_database.db";

    private Map<Integer, String> mDeviceTypeNameMap;
    private List<Pair<Integer, String>> mDeviceTypeNameList;

    public DeviceDataSource(@NonNull Context context) {
        mApplicationContext = context.getApplicationContext();
    }

    private synchronized void initDatabase() {
        if (mAppDatabase == null) {
            mAppDatabase = Room.databaseBuilder(
                    mApplicationContext,
                    AppDatabase.class,
                    mDBName
            ).fallbackToDestructiveMigration().build();
        }
    }

    private synchronized void initDataTypeName() {
        if (mDeviceTypeNameMap == null) {
            Map<Integer, String> map = Collections.synchronizedMap(new HashMap<>());
            map.put(DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, mApplicationContext.getString(R.string.blood_pressure_profile));
            mDeviceTypeNameMap = Collections.unmodifiableMap(map);

            List<Pair<Integer, String>> list = map.entrySet()
                    .stream()
                    .sorted((o1, o2) -> o2.getValue().compareTo(o1.getValue()))
                    .map(entry -> Pair.create(entry.getKey(), entry.getValue()))
                    .collect(Collectors.toList());
            mDeviceTypeNameList = Collections.unmodifiableList(Collections.synchronizedList(list));
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

    @NonNull
    public Single<Long> insertDevices(@NonNull Device device) {
        initDatabase();
        return mAppDatabase.getDeviceDao().insertDevices(device);
    }

    @NonNull
    public Completable deleteAllDevices() {
        initDatabase();
        return mAppDatabase.getDeviceDao().deleteAllDevices();
    }

    @Nullable
    public String getDeviceTypeName(int deviceType) {
        initDataTypeName();
        return mDeviceTypeNameMap.get(deviceType);
    }

    @NonNull
    public List<Pair<Integer, String>> provideDeviceTypeList() {
        initDataTypeName();
        return mDeviceTypeNameList;
    }

}

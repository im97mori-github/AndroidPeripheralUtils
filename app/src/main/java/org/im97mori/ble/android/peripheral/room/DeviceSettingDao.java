package org.im97mori.ble.android.peripheral.room;

import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;

import java.util.List;

import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.core.Single;

@Dao
public interface DeviceSettingDao {

    @Query("SELECT * from device_settings")
    Flowable<List<DeviceSetting>> loadAllDeviceSettings();

    @Query("SELECT * FROM device_settings WHERE device_setting_id = :id")
    Single<DeviceSetting> loadDeviceSettingByIdAsSingle(long id);

    @Query("SELECT * FROM device_settings WHERE device_setting_id = :id")
    Flowable<DeviceSetting> loadDeviceSettingByIdAsFlowable(long id);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insertDeviceSetting(DeviceSetting deviceSetting);

    @Delete(entity = DeviceSetting.class)
    Completable deleteDeviceSetting(DeviceSetting deviceSetting);

    @Query("DELETE from device_settings")
    Completable deleteAllDeviceSettings();

}

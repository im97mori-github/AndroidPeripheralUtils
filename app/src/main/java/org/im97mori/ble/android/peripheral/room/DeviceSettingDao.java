package org.im97mori.ble.android.peripheral.room;

import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;

import java.util.List;

import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Observable;
import io.reactivex.rxjava3.core.Single;

@Dao
public interface DeviceSettingDao {

    @Query("SELECT device_setting_id, device_setting_name, device_type, NULL as device_setting_data from device_setting")
    Observable<List<DeviceSetting>> loadAllDeviceSetting();

    @Query("SELECT * FROM device_setting WHERE device_setting_id = :id")
    Single<DeviceSetting> loadDeviceSettingById(long id);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insertDeviceSetting(DeviceSetting deviceSetting);

    @Delete(entity = DeviceSetting.class)
    Completable deleteDeviceSetting(DeviceSetting deviceSetting);

    @Query("DELETE from device_setting")
    Completable deleteAllDeviceSetting();

}

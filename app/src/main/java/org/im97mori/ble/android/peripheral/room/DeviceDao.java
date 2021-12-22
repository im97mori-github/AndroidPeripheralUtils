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
public interface DeviceDao {

    @Query("SELECT * from devices")
    Flowable<List<Device>> loadDevices();

    @Query("SELECT * FROM devices WHERE deviceid = :id")
    Single<Device> loadDeviceById(long id);

    @Query("SELECT * FROM devices WHERE deviceid = :id")
    Flowable<Device> loadDeviceByIdFlowable(long id);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    Completable insertDevice(Device device);

    @Delete(entity = Device.class)
    Completable deleteDevice(Device device);

    @Query("DELETE from devices")
    Completable deleteAllDevices();

}

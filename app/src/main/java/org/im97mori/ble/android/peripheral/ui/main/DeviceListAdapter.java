package org.im97mori.ble.android.peripheral.ui.main;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.room.Device;

import java.util.List;

public class DeviceListAdapter extends ArrayAdapter<Device> {

    private final LayoutInflater mLayoutInflater;

    private final List<Device> mDeviceList;

    public DeviceListAdapter(@NonNull Context context, @NonNull List<Device> deviceList) {
        super(context, 0, deviceList);
        mDeviceList = deviceList;
        mLayoutInflater = LayoutInflater.from(context);
    }

    public void setDeviceList(@NonNull List<Device> deviceList) {
        mDeviceList.clear();
        mDeviceList.addAll(deviceList);
        notifyDataSetChanged();
    }

    @Override
    public long getItemId(int position) {
        return mDeviceList.get(position).getId();
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        Device device = getItem(position);
        if (convertView == null) {
            convertView = mLayoutInflater.inflate(R.layout.main_grid, parent, false);
        }
        TextView textView = convertView.findViewById(R.id.grid_text);
        textView.setText(device.getDeviceSettingName());
        textView.setCompoundDrawablesRelativeWithIntrinsicBounds(0, R.drawable.ic_launcher_foreground, 0, 0);
        return convertView;
    }
}

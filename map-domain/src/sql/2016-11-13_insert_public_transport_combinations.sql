insert into public_transport_combination (fromStopId, toStopId, fromTravelInfoId,
    toTravelInfoId, distance, fromCoordinate, toCoordinate)
    select
        candidateCombination.stop_id_from,
        candidateCombination.stop_id_to,
        candidateCombination.travel_info_id_from,
        candidateCombination.travel_info_id_to,
        MIN(candidateCombination.distance) min_distance,
        candidateCombination.stop_coordinate_from,
        candidateCombination.stop_coordinate_to
    from
        (select tiFrom.id travel_info_id_from, tiTo.id travel_info_id_to, sFrom.id stop_id_from, sTo.id stop_id_to, ST_Distance(sFrom.coordinate, sTo.coordinate) distance, sFrom.coordinate stop_coordinate_from, sTo.coordinate stop_coordinate_to
            from
                stop sFrom,
                stop sTo,
                travel_info tiFrom,
                travel_info tiTo
            where
                (sFrom.id <> sTo.id
                    AND ST_Distance(sFrom.coordinate, sTo.coordinate) <= 0.00007848061)
                AND tiFrom.id != tiTo.id
                AND tiFrom.id = sFrom.travelInfoId
                AND tiTo.id = sTo.travelInfoId) candidateCombination
    group by stop_id_from, travel_info_id_to;